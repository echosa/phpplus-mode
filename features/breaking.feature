Feature: Breaking
  In order to break properly
  As a user
  I want to have php+-mode break properly
  
  Scenario: Breaking with arrays and method chains
    Given I am in buffer "test.php"
    And I clear the buffer
    And I insert: 
    """
    <?php
    class EmployeeData
    {
        public function test()
        {
            if (true) {
                $testsArray[$test->pk]['results'] = $resultsTable->fetchAll($select)->toArray(); 
            }
        }
    }
    """
    And I print the buffer string
    And I turn on php+-mode
    And I break the statement at line 7
    And I print the buffer string
    Then I should see "= $resultsTable->fetchAll($select)->toArray();"

  Scenario: Indent semicolon already on a newline
    Given I am in buffer "test.php"
    And I clear the buffer
    And I insert: 
    """
    <?php
    class EmployeeData
    {
        public function test()
        {
            $select = $this->select()->setIntegrityCheck(false)->from(array('m' => $this->_name), array('pk', 'date', 'notes'))->join(array('r' => 'occHealthMedicalTestResults'), 'm.result = r.pk', array('result', 'test'))->join(array('t' => 'occHealthMedicalTests'), 'r.test = t.pk', array('name'))->where('m.person = ?', $person->getPk())->order('date DESC');
        }
    }
    """
    And I print the buffer string
    And I turn on php+-mode
    And I break the statement at line 6
    And I print the buffer string
    Then I should see "array('name')"
