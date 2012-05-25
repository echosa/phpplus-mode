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
     * Process Camtasia Relay Metadata file
     *
     * @param File $parent_file parent file
     * @param File $file        file
     *
     * @author Michael Dwyer <mdwyer@tamu.edu>
     *
     * @return void
     */
    static public function camtasiaRelayMetadata(File $parent_file,
        File $file
    ) {
        $dbIface = $GLOBALS['db'];

        $media_files = array();
        $xml_file = null;

        $base_file_parts = DAV_CreateScripts::processFileName($file->file_name);

        $regex = '/^' . preg_quote($base_file_parts->title) . '/';
        $matches = $parent_file->getMatchingChildren($regex);

        foreach ($matches as $match) {
            if ($match->ext == 'xml') {
                $xml_file = $match;
            } else {
                $media_files[] = $match;
            }
        }

        if ($xml_file && count($media_files) > 0) {
            DAV_CreateScripts::processXml($dbIface, $xml_file, $media_files);
        }

        if (strtolower($file->ext) != 'xml') {
            $file->rename(
                implode(
                    '.',
                    array(
                        $base_file_parts->title,
                        $base_file_parts->ext
                    )
                )
            );

            DAV_CreateScripts::processMedia(
                $file,
                $parent_file,
                $base_file_parts
            );
        }
    }

    /**
     * Split the file name into it's constituent parts
     *
     * @param string $file_name file name
     *
     * @author Michael Dwyer <mdwyer@tamu.edu>
     *
     * @return object
     */
    static public function processFileName($file_name)
    {
        $base_file_parts = new stdClass();
        $match = array();

        preg_match('/(.*)\.([^.]*)$/', $file_name, $match);

        $base_file_parts->name = $match[1];
        $base_file_parts->ext = $match[2];
        $file_name_parts = array_reverse(
            explode('_-_', $base_file_parts->name)
        );

        $base_file_date_parts = explode('_', array_shift($file_name_parts));
        $base_file_parts->date = $base_file_date_parts[0];
        $base_file_parts->time = $base_file_date_parts[1];

        $base_file_parts->format = array_shift($file_name_parts);
        $base_file_parts->title = str_replace(
            '_',
            ' ',
            implode('_-_', array_reverse($file_name_parts))
        );

        $base_file_parts->description = null;
        $base_file_parts->keywords = null;

        return $base_file_parts;
    }

    /**
     * Create a NetID User object from a NetID
     *
     * @param User &$recorder recorder
     *
     * @author Michael Dwyer <mdwyer@tamu.edu>
     *
     * @return void
     */
    static protected function createNetIDRecorder(User &$recorder)
    {
        $d_info = NetID::getTAMUDirectoryInfo(
            $recorder->username,
            'netid',
            false
        );

        if (NetID::isValidDirectoryInfo($d_info)) {
            $recorder_username = $d_info->uid[0];

            try {
                $recorder = new User($recorder_username);
            } catch (User_Exception $e) {
                throw new Sabre_DAV_Exception(
                    'Error creating NetID recorder User object.',
                    0,
                    $e
                );
            }
        }

    }
}
?>