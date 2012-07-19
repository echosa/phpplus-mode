Feature: Line Breaking
  In order to have proper automatic line breaking

  Scenario: ERT Breaking test 3-1
    Given I am in buffer "Test_Breaking3-1.php"
    And I turn on php+-mode
    And I insert:
    """
    <?php
    class DAV_CreateScripts
    {
        static protected function moveFileToRecorderAccount(File $file, User $recorder) {
            $matches = $recorder_home_file->getMatchingChildren('/' . $dest_dir_name . '/');
            $status->addMessage(LOG_DEBUG, 'Recorder home file: ' . $recorder_home_file->full_path);
            $status->addMessage(LOG_DEBUG, 'Matches: ' . dump_var(array_map(function($x) {return $x->file_name;}, $matches)));
            if (count($matches) == 0) {
                try {
                    $dest_dir = $recorder_home_file->createFolder($dest_dir_name, 'Camtasia Relay Incoming (' . $relay_username . ')', 'Incoming directory for Camtasia Relay recording from ' . $relay_username . '.');
                } catch (File_Exception $e) {
                    throw new Sabre_DAV_Exception('Error creating Camtasia Relay Incoming directory ' . $dest_dir_name . ' under home directory for account ' . $recorder_username . '.  Will continue to publish stream.  The file may be moved later.', 0, $e);
                }
            }
            if ($dest_dir) {
                try {
                    $file->move($dest_dir->file_id);
                } catch (File_Exception $e) {
                    throw new Sabre_DAV_Exception('Error creating moving file to Camtasia Relay Incoming directory ' . $dest_dir_name . ' under home directory for account ' . $recorder_username . '.  Will continue to publish stream.  The file may be moved later.', 0, $e);
                }
                try {
                    $file->save();
                } catch (File_Exception $e) {
                    throw new Sabre_DAV_Exception('Error changing owner username of uploaded file.', 0, $e);
                }
            }
        }
        static protected function processMedia(File $file, File $parentFile, $fileNameParts) {
            $file->content_type_id = getValidValue(array($file->content_type_id, 1), 'is_valid_db_id');
            $file->copyright_type_id = getValidValue(array($file->copyright_type_id, 2), 'is_valid_db_id');
            try {
                $recorder = new User($recorder_username);
            } catch (User_Exception $e) {
                throw new Sabre_DAV_Exception('Error creating local recorder User object.', 0, $e);
            }
        }
    }
    """
    And I break the statement at line 34
    Then I should see:
    """
                throw new Sabre_DAV_Exception(
                    'Error creating local recorder User object.',
                    0,
                    $e
                );
    """
    And I break the statement at line 30
    Then I should see:
    """
            $file->copyright_type_id = getValidValue(
                array($file->copyright_type_id, 2),
                'is_valid_db_id'
            );
    """
    And I break the statement at line 29
    Then I should see:
    """
            $file->content_type_id = getValidValue(
                array($file->content_type_id, 1),
                'is_valid_db_id'
            );
    """
    And I break the statement at line 28
    Then I should see:
    """
        static protected function processMedia(File $file, File $parentFile,
            $fileNameParts
        ) {
    """
    And I break the statement at line 24
    Then I should see:
    """
                    throw new Sabre_DAV_Exception(
                        'Error changing owner username of uploaded file.',
                        0,
                        $e
                    );
    """
    And I break the statement at line 19
    Then I should see:
    """
                    throw new Sabre_DAV_Exception(
                        'Error creating moving file to Camtasia Relay Incoming '
                        . 'directory ' . $dest_dir_name
                        . ' under home directory for account ' . $recorder_username
                        . '.  Will continue to publish stream.  The file may be '
                        . 'moved later.',
                        0,
                        $e
                    );
    """
    And I break the statement at line 12
    Then I should see:
    """
                    throw new Sabre_DAV_Exception(
                        'Error creating Camtasia Relay Incoming directory '
                        . $dest_dir_name . ' under home directory for account '
                        . $recorder_username
                        . '.  Will continue to publish stream.  The file may be '
                        . 'moved later.',
                        0,
                        $e
                    );
    """
    And I break the statement at line 10
    Then I should see:
    """
                    $dest_dir = $recorder_home_file->createFolder(
                        $dest_dir_name,
                        'Camtasia Relay Incoming (' . $relay_username . ')',
                        'Incoming directory for Camtasia Relay recording from '
                        . $relay_username . '.'
    """
    And I break the statement at line 7
    Then I should see:
    """
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
    """
    And I break the statement at line 6
    Then I should see:
    """
            $status->addMessage(
                LOG_DEBUG,
                'Recorder home file: ' . $recorder_home_file->full_path
            );
    """
    And I break the statement at line 5
    Then I should see:
    """
            $matches = $recorder_home_file->getMatchingChildren(
                '/' . $dest_dir_name . '/'
            );
    """
    And I break the statement at line 4
    Then I should see:
    """
        static protected function moveFileToRecorderAccount(File $file,
            User $recorder
        ) {
    """    
