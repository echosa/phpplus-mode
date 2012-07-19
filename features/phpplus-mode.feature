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
    And I press "C-c b s"
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
            $this->someCall('someString');

            $select = $this->select()->where('person = ?', $person->pk)->order('date DESC');
        }
    }
    """
    And I go to line "8"
    And I press "C-c b s"
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
    And I press "C-c b s"
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
            $this->view->form = new Occhealth_Form_Personnel();
            if ($this->getRequest()->isPost()) {
                if ($this->view->form->isValid($this->getRequest()->getParams())) {
                    $person = new Occhealth_Model_Personnel($this->getRequest()->getParams());
                    $person->save();
                    $this->_helper->flashMessenger($person->getName() . ' added to Occupation Health.');
                }
            }
        }
    }
    """
    And I go to line "11"
    And I press "C-c b s"
    Then I should see:
    """
                    $this->_helper->flashMessenger(
                        $person->getName() . ' added to Occupation Health.'
                    );
    """
    And I go to line "9"
    And I press "C-c b s"
    Then I should see:
    """
                    $person = new Occhealth_Model_Personnel(
                        $this->getRequest()->getParams()
                    );
    """
