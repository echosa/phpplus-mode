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
    And I break the statement at line 6
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
    And I break the statement at line 6
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
    And I break the statement at line 6
    Then I should see:
    """
            $string = 'Is Certified: '
                . $this->view->yesOrNo($location->isCertified)
                . '<br />Date Certified: ';
    """

