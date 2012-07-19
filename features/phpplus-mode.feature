Feature: Statement Cont Lineup
  In order to line up a statement continuation
  As a user
  I want to press the tab key
  
  Scenario: Second line of anonymous function
    Given I am in buffer "test.php"
    And I insert: 
    """
    <?php
    class EmployeeData
    {
        protected function escape($string)
        {
            join(
                '',
                array_map(
                    split('', $string),
                    function ($char) {
                        return (in_array($char, array('\\', '\'')) ? '\\' : '')
                . $char;
                    }
                )
            );
        }
    }
    """
    And I turn on php+-mode
    And I go to line "12"
    And I press "TAB"
    Then the cursor should be before ". $char;"
    And I should be at column 24

Feature: Font Locking
  In order to see properly font locked text
  As a user
  I want to see keywords highlighted properly

  Scenario: Font locking of anonymous function and return
    Given I am in buffer "test.php"
    And I turn on font-lock-fontify-buffer
    And I go to line "10"
    And I go to the beginning of line text
    Then I should see the face font-lock-keyword-face
    And I go to line "11"
    And I go to the beginning of line text
    Then I should see the face font-lock-keyword-face

Feature: Line Breaking
  In order to have proper automatic line breaking
 
  Scenario: ERT breaking test 1
    Given I am in buffer "Test_Breaking.php"
    And I turn on php+-mode
    And I insert:
    """
    <?php
    class Zfmode_Test_Breaking
    {
        public function method1()
        {
            $person = new Occhealth_Model_Personnel($this->getRequest()->getParams());
        }
    }
    """
    And I go to line "6"
    And I break the current statement
    Then I should see:
    """
            $person = new Occhealth_Model_Personnel(
                $this->getRequest()->getParams()
            );
    """
    And I clear the buffer
    And I insert:
    """
    <?php
    class Zfmode_Test_Breaking
    {
        public function method2()
        {
            $select = $this->select()->where('person = ?', $person->pk)->order('date DESC');
        }
    }
    """
    And I go to line "6"
    And I break the current statement
    Then I should see:
    """
            $select = $this->select()->where('person = ?', $person->pk)
                ->order('date DESC');
    """
    And I clear the buffer
    And I insert:
    """
    <?php
    class Zfmode_Test_Breaking
    {
        private function _method3()
        {
            $string = 'Is Certified: ' . $this->view->yesOrNo($location->isCertified) . '<br />Date Certified: ';
        }
    }
    """
    And I go to line "6"
    And I break the current statement
    Then I should see:
    """
            $string = 'Is Certified: '
                . $this->view->yesOrNo($location->isCertified)
                . '<br />Date Certified: ';
    """

  Scenario ERT breaking test 2
    Given I am in buffer "Test_Breaking2.php"
    And I turn on php+-mode
    And I insert:
    """
    <?php
    class Occhealth_PersonnelController extends Zend_Controller_Action
    {
        public function addAction()
        {
            if ($this->getRequest()->isPost()) {
                if ($this->view->form->isValid($this->getRequest()->getParams())) {
                    $person = new Occhealth_Model_Personnel($this->getRequest()->getParams());
                    $this->_helper->flashMessenger($person->getName() . ' added to Occupation Health.');
                }
            }
        }
    }
    """
    And I go to line "9"
    And I break the current statement
    Then I should see:
    """
                    $this->_helper->flashMessenger(
                        $person->getName() . ' added to Occupation Health.'
                    );
    """
    And I go to line "8"
    And I break the current statement
    Then I should see:
    """
                    $person = new Occhealth_Model_Personnel(
                        $this->getRequest()->getParams()
                    );
    """

  Scenario ERT Breaking test 3
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
