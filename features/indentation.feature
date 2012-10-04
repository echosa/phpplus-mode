Feature: Indentation
  In order to indent properly
  As a user
  I want to have php+-mode indent properly
  
  Scenario: Pressing return at semicolon should indent properly
    Given I am in buffer "test.php"
    And I clear the buffer
    And I insert: 
    """
    <?php
    class EmployeeData
    {
        public function test()
        {
            $this->method()->chain();
        }
    }
    """
    And I turn on php+-mode
    And I place the cursor after "chain()"
    And I press "<return>"
    Then I should be at column 12

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
            $this->method()->chain()
    ;
        }
    }
    """
    And I turn on php+-mode
    And I go to line "7"
    And I press "TAB"
    Then I should be at column 12

  Scenario: Case labels
    Given I am in buffer "test.php"
    And I clear the buffer
    And I insert:
    """
    <?php
    switch (test) {
    case "foo":
    """
    And I turn on php+-mode
    And I go to line "3"
    and I press "TAB"
    Then I should be at column 4
