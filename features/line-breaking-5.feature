Feature: Line Breaking
  In order to have proper automatic line breaking

  Scenario: ERT Breaking test 3-2
    Given I am in buffer "Test_Breaking3-2.php"
    And I turn on php+-mode
    And I insert:
    """
    <?php
    class DAV_CreateScripts
    {
        static protected function processXML(DB $dbIface, File $xmlFile, array $mediaFiles = array()) {
            $xml = simplexml_load_file($xmlFile->full_path);
            if (!$xml) {
                throw new Sabre_DAV_Exception('Error parsing Camtasia Relay metadata XML.');
            }
            $title = $xml->title[0];
            $description = $xml->description[0];
            try {
                $xmlFile->delete();
            } catch (File_Exception $e) {
                throw new Sabre_DAV_Exception('Error removing Camtasia Relay metadata XML.', 0, $e);
            }
            $sql = 'UPDATE Files SET title = ?, description = ? WHERE file_id = ?';
            foreach ($mediaFiles as $m_file) {
                $params = array($title, $description, $m_file->file_id);
                try {
                    $dbIface->runQuery($sql, $params);
                } catch (DB_Exception $e) {
                    throw new Sabre_DAV_Exception('Error updating title and description in database.', 0, $e);
                }
            }
        }
        static protected function publishStream(User $recorder, File $file)
        {
            $dest_dir_name = $_SESSION['user_info']->username;
            $result = null;
            try {
                $result = $file->publish(1);
            } catch (File_Exception $e) {
                throw new Sabre_DAV_Exception('Error publishing file ' . $file->file_name . '.', 0, $e);
            }
            if ($result) {
                $stream = null;
                try {
                    $stream = new Stream($result);
                } catch (Stream_Exception $e) {
                    throw new Sabre_DAV_Exception('Error creating object for stream id ' . $result . '.', 0, $e);
                }
                if ($stream) {
                    $url = explode('/', $stream->url);
                    array_shift($url);
                    array_shift($url);
                    array_shift($url);
                    $url = 'https://mediamatrix.tamu.edu/' . join('/', $url);
                    $mail_to = $recorder->email_address;
                    if (is_valid_input($mail_to)) {
                        $mail_message = 'You have a new stream available!  The Camtasia Relay Service ' . $dest_dir_name . ' has published your stream at the url: ' . $url . ' .  If you have a MediaMatrix account, the file should now reside in your account under the' . $dest_dir_name . ' directory.';
                        $mail_subject = 'New stream from Camtasia Relay Service ' . $dest_dir_name . '!';
                        /* $mail_from = "support@ims.tamu.edu"; */
                        $mail_sent = mail($mail_to, $mail_subject, $mail_message);
                        if (!$mail_sent) {
                            throw Sabre_DAV_Exception('Error sending email to user ' . $recorder->username . 'at address ' . $mail_to . 'telling them that their stream at url ' . $url . ' was ready.');
                        }
                    }
                }
            }
        }
    }
    """
    And I break the statement at line 55
    Then I should see:
    """
                            throw Sabre_DAV_Exception(
                                'Error sending email to user '
                                . $recorder->username . 'at address ' . $mail_to
                                . 'telling them that their stream at url ' . $url
                                . ' was ready.'
                            );
    """
    And I break the statement at line 51
    Then I should see:
    """
                        $mail_subject = 'New stream from Camtasia Relay Service '
                            . $dest_dir_name . '!';
    """
    And I break the statement at line 50
    Then I should see:
    """
                        $mail_message = 'You have a new stream available!  The '
                            . 'Camtasia Relay Service ' . $dest_dir_name
                            . ' has published your stream at the url: ' . $url
                            . ' .  If you have a MediaMatrix account, the file '
                            . 'should now reside in your account under the'
                            . $dest_dir_name . ' directory.';
    """
    And I break the statement at line 40
    Then I should see:
    """
                    throw new Sabre_DAV_Exception(
                        'Error creating object for stream id ' . $result . '.',
                        0,
                        $e
                    );
    """
    And I break the statement at line 33
    Then I should see:
    """
                throw new Sabre_DAV_Exception(
                    'Error publishing file ' . $file->file_name . '.',
                    0,
                    $e
                );
    """
    And I break the statement at line 22
    Then I should see:
    """
                    throw new Sabre_DAV_Exception(
                        'Error updating title and description in database.',
                        0,
                        $e
                    );
    """
    And I break the statement at line 14
    Then I should see:
    """
                throw new Sabre_DAV_Exception(
                    'Error removing Camtasia Relay metadata XML.',
                    0,
                    $e
                );
    """
    And I break the statement at line 7
    Then I should see:
    """
                throw new Sabre_DAV_Exception(
                    'Error parsing Camtasia Relay metadata XML.'
                );
    """
    And I break the statement at line 4
    Then I should see:
    """
        static protected function processXML(
            DB $dbIface,
            File $xmlFile,
            array $mediaFiles = array()
        ) {
    """
