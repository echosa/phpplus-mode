<?php
/**
 * zf-mode Test Class
 *
 * PHP Version 5.3
 *
 * @category   ZfMode
 * @package    Test
 * @subpackage Class
 *
 * @author     Brian Zwahr <bzwahr@tamu.edu>
 * @copyright  2011 Texas A&M University
 * @license    http://www.gnu.org/licenses/gpl-3.0.html  GPLv3
 * @version    SVN: $Id: $
 * @link       http://www.tamu.edu
 * @see        Zfmode_Test
 */

/**
 * zf-mode Test Class
 *
 * @category   ZfMode
 * @package    Test
 * @subpackage Class
 *
 * @author     Brian Zwahr <bzwahr@tamu.edu>
 * @license    http://www.gnu.org/licenses/gpl-3.0.html  GPLv3
 * @link       http://www.tamu.edu
 * @see        Zfmode_Test.php
 */
class Zfmode_Test_Alignment
{
    /**
     * First test method.
     *
     * First test method. Should be public.
     *
     * @author Brian Zwahr <bzwahr@tamu.edu>
     *
     * @return void
     */
    public function method1()
    {
        $array = array(
            'Some Text' => 'ohai',
            'Some Array' => array(
                'text' => $variable,
                'third' => array(
                    'something' => 'here',
                    'other'     => 'stuff,'
                ),
                'foobarasdf' => 1,
            ),
            'Some Really Long Name' => array(
                'text'       => $this->someMethod(),
                'breakAfter' => 1,
            ),
            'Yet another one' => 0,
            'Lots of levels' => array(
                'text' => $ohai,
                'third' => array(
                    'something' => 'here',
                    'fourth' => array(
                        'this' => 'is',
                        'a' => 'fourth',
                        'level' => 'array',
                        'fifth' => array(
                            'for'     => 'great',
                            'justice' => 'yeah',
                        ),
                    ),
                    'other' => 'stuff',
                ),
                'breakasdf' => 1,
            ),
        );
    }

    /**
     * Test submitting the edit action with valid data.
     *
     * @author Brian Zwahr <bzwahr@tamu.edu>
     *
     * @return void
     */
    public function testEditActionSubmitValid()
    {
        $table = new Animal_Model_DbTable_AnimalSpecies();
        $info = $table->find(1)->current();
        $this->actionTest(
            array('params' => array('id' => 1), 'post' => $info->toArray())
        )
            ->xpathEqualsTest(
                '//div[@id="messages"]',
                "Species 1's information updated."
            );
    }

    /**
     * Test the search action default
     *
     * @author Brian Zwahr <bzwahr@tamu.edu>
     *
     * @return void
     */
    public function testSearchAction()
    {
        $this->actionTest()
            ->zendFormTest()
            ->xpathNotTest('//div[@class="searchResults"]');
    }

}
