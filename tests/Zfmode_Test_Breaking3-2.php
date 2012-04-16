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
     * Process XML
     *
     * @param DB    $dbIface    database interface
     * @param File  $xmlFile    XML file
     * @param array $mediaFiles media files
     *
     * @author Michael Dwyer <mdwyer@tamu.edu>
     *
     * @return void
     */
    static protected function processXML(DB $dbIface, File $xmlFile,
        array $mediaFiles = array()
    ) {
        $xml = simplexml_load_file($xmlFile->full_path);

        if (!$xml) {
            throw new Sabre_DAV_Exception(
                'Error parsing Camtasia Relay metadata XML.'
            );
        }

        $title = $xml->title[0];
        $description = $xml->description[0];

        try {
            $xmlFile->delete();
        } catch (File_Exception $e) {
            throw new Sabre_DAV_Exception(
                'Error removing Camtasia Relay metadata XML.',
                0,
                $e
            );
        }

        $sql = 'UPDATE Files SET title = ?, description = ? WHERE file_id = ?';

        foreach ($mediaFiles as $m_file) {
            $params = array($title, $description, $m_file->file_id);

            try {
                $dbIface->runQuery($sql, $params);
            } catch (DB_Exception $e) {
                throw new Sabre_DAV_Exception(
                    'Error updating title and description in database.',
                    0,
                    $e
                );
            }
        }
    }

    /**
     * Publish stream and email link
     *
     * @param User $recorder recorder
     * @param File $file     file
     *
     * @author Michael Dwyer <mdwyer@tamu.edu>
     *
     * @return void
     */
    static protected function publishStream(User $recorder, File $file)
    {
        $dest_dir_name = $_SESSION['user_info']->username;
        $result = null;

        try {
            $result = $file->publish(1);
        } catch (File_Exception $e) {
            throw new Sabre_DAV_Exception(
                'Error publishing file ' . $file->file_name . '.',
                0,
                $e
            );
        }

        if ($result) {
            $stream = null;

            try {
                $stream = new Stream($result);
            } catch (Stream_Exception $e) {
                throw new Sabre_DAV_Exception(
                    'Error creating object for stream id ' . $result . '.',
                    0,
                    $e
                );
            }

            if ($stream) {
                $url = explode('/', $stream->url);
                array_shift($url);
                array_shift($url);
                array_shift($url);
                $url = 'https://mediamatrix.tamu.edu/' . join('/', $url);
                $mail_to = $recorder->email_address;

                if (is_valid_input($mail_to)) {
                    $mail_message = 'You have a new stream available!  The '
                        . 'Camtasia Relay Service ' . $dest_dir_name
                        . ' has published your stream at the url: ' . $url
                        . ' .  If you have a MediaMatrix account, the file '
                        . 'should now reside in your account under the'
                        . $dest_dir_name . ' directory.';

                    $mail_subject = 'New stream from Camtasia Relay Service '
                        . $dest_dir_name . '!';

                    /* $mail_from = "support@ims.tamu.edu"; */

                    $mail_sent = mail($mail_to, $mail_subject, $mail_message);

                    if (!$mail_sent) {
                        throw Sabre_DAV_Exception(
                            'Error sending email to user '
                            . $recorder->username . 'at address ' . $mail_to
                            . 'telling them that their stream at url ' . $url
                            . ' was ready.'
                        );
                    }
                }
            }

        }
    }
}
?>