Feature: Line Breaking
  In order to have proper automatic line breaking

  Scenario: ERT breaking test 2
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
    And I break the statement at line 9
    Then I should see:
    """
                    $this->_helper->flashMessenger(
                        $person->getName() . ' added to Occupation Health.'
                    );
    """
    And I break the statement at line 8
    Then I should see:
    """
                    $person = new Occhealth_Model_Personnel(
                        $this->getRequest()->getParams()
                    );
    """

