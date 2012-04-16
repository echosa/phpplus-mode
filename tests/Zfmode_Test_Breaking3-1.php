<?php
/**
 * File creation script container
 *
 * PHP Version 5.3
 *
 * @category   MediaMatrix
 * @package    DAV
 * @subpackage CreateScripts
 *
 * @author     Michael Dwyer <mdwyer@tamu.edu>
 * @copyright  2010 Texas A&M University
 * @license    http://www.gnu.org/licenses/gpl-3.0.html  GPLv3
 * @version    SVN: $Id: File.inc 1507 2010-08-10 18:37:30Z mdwyer $
 * @link       http://mediamatrix.tamu.edu
 */

/**
 * File creation script container
 *
 * This class functions as a container of functions that will be run on
 * files created through the DAV interface DAV_Directory::createFile().
 * What functions if any are run are determined via the
 * FileCreateScriptsAccess and AssignedFileCreateScripts tables in the
 * database.
 *
 * @category   MediaMatrix
 * @package    DAV
 * @subpackage CreateScripts
 *
 * @author     Michael Dwyer <mdwyer@tamu.edu>
 * @license    http://www.gnu.org/licenses/gpl-3.0.html GPLv3
 * @link       http://mediamatrix.tamu.edu
 */
class DAV_CreateScripts
{
    /**
     * Move file to recorder's account
     *
     * @param File $file     file
     * @param User $recorder recorder
     *
     * @author Michael Dwyer <mdwyer@tamu.edu>
     *
     * @return void
     */
    static protected function moveFileToRecorderAccount(File $file,
        User $recorder
    ) {
        $relay_username = $_SESSION['user_info']->username;
        $dest_dir_name = 'Camtasia Relay via ' . $relay_username;

        $recorder_username = $recorder->username;
        $recorder_home_file = $recorder->home_file;
        $dest_dir = null;
        $matches = $recorder_home_file->getMatchingChildren(
            '/' . $dest_dir_name . '/'
        );

        $status = $GLOBALS['status'];
        $status->addMessage(LOG_DEBUG, 'Dest dir name: ' . $dest_dir_name);
        $status->addMessage(
            LOG_DEBUG,
            'Recorder home file: ' . $recorder_home_file->full_path
        );

        $status->addMessage(
            LOG_DEBUG,
            'Matches: '
            . dump_var(
                array_map(
                    function($x)
                    {
                        return $x->file_name;
                    },
                    $matches
                )
            )
        );

        if (count($matches) == 0) {
            try {
                $dest_dir = $recorder_home_file->createFolder(
                    $dest_dir_name,
                    'Camtasia Relay Incoming (' . $relay_username . ')',
                    'Incoming directory for Camtasia Relay recording from '
                    . $relay_username . '.'
                );
            } catch (File_Exception $e) {
                throw new Sabre_DAV_Exception(
                    'Error creating Camtasia Relay Incoming directory '
                    . $dest_dir_name . ' under home directory for account '
                    . $recorder_username
                    . '.  Will continue to publish stream.  The file may be '
                    . 'moved later.',
                    0,
                    $e
                );
            }
        } else {
            $dest_dir = $matches[0];
        }

        if ($dest_dir) {
            try {
                $file->move($dest_dir->file_id);
            } catch (File_Exception $e) {
                throw new Sabre_DAV_Exception(
                    'Error creating moving file to Camtasia Relay Incoming '
                    . 'directory ' . $dest_dir_name
                    . ' under home directory for account ' . $recorder_username
                    . '.  Will continue to publish stream.  The file may be '
                    . 'moved later.',
                    0,
                    $e
                );
            }

            $file->owner_username = $recorder_username;

            try {
                $file->save();
            } catch (File_Exception $e) {
                throw new Sabre_DAV_Exception(
                    'Error changing owner username of uploaded file.',
                    0,
                    $e
                );
            }
        }
    }

    /**
     * Process Media
     *
     * @param File   $file          file
     * @param File   $parentFile    parent file
     * @param object $fileNameParts file name parts
     *
     * @author Michael Dwyer <mdwyer@tamu.edu>
     *
     * @return void
     */
    static protected function processMedia(File $file, File $parentFile,
        $fileNameParts
    ) {
        /* Sensible default so that we can steam */
        foreach (array('title', 'description', 'keywords') as $field) {
            $file->$field = getValidValue(
                array($file->$field, $fileNameParts->$field, $file->file_name)
            );
        }

        $file->content_type_id = getValidValue(
            array($file->content_type_id, 1),
            'is_valid_db_id'
        );

        $file->copyright_type_id = getValidValue(
            array($file->copyright_type_id, 2),
            'is_valid_db_id'
        );

        /*
          Here we determine who the recorder is.  If they have a MediaMatrix
          account, we move the file to their account.
        */

        $recorder_username = basename($parentFile->file_name);
        $recorder = null;

        try {
            $recorder = new User($recorder_username);
        } catch (User_Exception $e) {
            throw new Sabre_DAV_Exception(
                'Error creating local recorder User object.',
                0,
                $e
            );
        }

        if ($recorder->new_user) {
            DAV_CreateScripts::createNetIDRecorder($recorder);
        }

        if (!$recorder->new_user && ($recorder->user_level_id < 2)) {
            DAV_CreateScripts::moveFileToRecorderAccount($file, $recorder);
        }

        /* Here we publish the file and email a link to the recorder. */
        DAV_CreateScripts::publishStream($recorder, $file);
    }
}
?>