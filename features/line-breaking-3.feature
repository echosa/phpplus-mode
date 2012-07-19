Feature: Line Breaking
  In order to have proper automatic line breaking

  Scenario: ERT Breaking test 3
    Given I am in buffer "Test_Breaking3.php"
    And I turn on php+-mode
    And I insert:
    """
    <?php
    class DAV_CreateScripts
    {
        static public function camtasiaRelayMetadata(File $parent_file, File $file) {
            if (strtolower($file->ext) != 'xml') {
                $file->rename(implode('.', array($base_file_parts->title, $base_file_parts->ext)));
                DAV_CreateScripts::processMedia($file, $parent_file, $base_file_parts);
            }
        }
        static public function processFileName($file_name)
        {
            $base_file_parts->title = str_replace('_', ' ', implode('_-_', array_reverse($file_name_parts)));
        }
        static protected function createNetIDRecorder(User &$recorder)
        {
            $d_info = NetID::getTAMUDirectoryInfo($recorder->username, 'netid', false);
            if (NetID::isValidDirectoryInfo($d_info)) {
                try {
                    $recorder = new User($recorder_username);
                } catch (User_Exception $e) {
                    throw new Sabre_DAV_Exception('Error creating NetID recorder User object.', 0, $e);
                }
            }
        }
    }
    """
    And I break the statement at line 21
    Then I should see:
    """
                    throw new Sabre_DAV_Exception(
                        'Error creating NetID recorder User object.',
                        0,
                        $e
                    );
    """
    And I break the statement at line 16
    Then I should see:
    """
            $d_info = NetID::getTAMUDirectoryInfo(
                $recorder->username,
                'netid',
                false
            );
    """
    And I break the statement at line 12
    Then I should see:
    """
            $base_file_parts->title = str_replace(
                '_',
                ' ',
                implode('_-_', array_reverse($file_name_parts))
            );
    """
    And I break the statement at line 7
    Then I should see:
    """
                DAV_CreateScripts::processMedia(
                    $file,
                    $parent_file,
                    $base_file_parts
                );
    """
    And I break the statement at line 6
    Then I should see:
    """
                $file->rename(
                    implode(
                        '.',
                        array($base_file_parts->title, $base_file_parts->ext)
                    )
                );
    """
    And I break the statement at line 4
    Then I should see:
    """
        static public function camtasiaRelayMetadata(File $parent_file,
            File $file
        ) {
    """
