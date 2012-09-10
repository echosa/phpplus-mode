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

  Scenario: Indent an inline anonymous function
    Given I am in buffer "test.php"
    And I clear the buffer
    And I insert:
    """
    <?php
    namespace Edu\Tamu\Vpr\DumpParser;
    spl_autoload_register(
        function ($className) {
            $className = ltrim($className, '\\');
        $fileName  = '';
        $namespace = '';
        if ($lastNsPos = strripos($className, '\\')) {
            $namespace = substr($className, 0, $lastNsPos);
            $className = substr($className, $lastNsPos + 1);
            $fileName  = str_replace('\\', DIRECTORY_SEPARATOR, $namespace) . DIRECTORY_SEPARATOR;
        }
        $fileName .= str_replace('_', DIRECTORY_SEPARATOR, $className) . '.php';
        require $fileName;
        }
    );
    """
    And I turn on php+-mode
    And I go to line "6"
    And I press "TAB"
    Then I should be at column 8


  Scenario: Indent class function closing brace
    Given I am in buffer "test.php"
    And I clear the buffer
    And I insert:
    """
    <?php
    class PeopleDumpToRecordsTest extends PHPUnit_Framework_TestCase
    {
        private $recordFields = [
            'tamuEduPersonUIN',
            'tamuEduPersonNetID',
            'uid',
            'sn',
            'givenName',
            'displayName',
            'tamuEduPersonOfficialName',
            'mail',
            'postalAddress',
            'mailStop',
            'localityName',
            'stateOrProvinceName',
            'postalCode',
            'telephoneNumber',
            'cn',
            'title',
            'tamuEduPersonDepartmentName',
            'tamuEduPersonPrimaryMemberName',
            'tamuEduPersonAffiliation'
        ];
        protected function setUp()
        {
            $this->dump = new PeopleDumpToRecords;
        }
    """
    And I turn on php+-mode
    And I go to line "28"
    And I press "TAB"
    Then I should be at column 4
