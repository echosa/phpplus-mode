<?php
class TestClass
{
    /**
     * Auxillary function to create a correct file structure on disk
     *
     * @param string $path         file path
     * @param int    $file_type_id file type
     * @param mixed  $data         data stream or file name or array of file
     * names
     * @param bool   $replace      remove old file(s) before writing new?
     *
     * @access    public
     * @static
     */
    static public function writeDataToDisk($path, $file_type_id, $data,
        $replace = true
    ) {
        switch ($file_type_id) {
        case 1:
            $fh = fopen($path, 'w');

            if ($fh === false) {
                throw new File_Exception('Error creating new file on disk.');
            }

            if (!is_resource($data)) {
                if (is_array($data)) {
                    $data = $data[0]['tmp_name'];
                }

                $data = fopen($data, 'r');

                if ($data === false) {
                    throw new File_Exception('Error opening data file.');
                }
            }

            $amount_written = stream_copy_to_stream($data, $fh);

            fclose($fh);

            if ($amount_written === false) {
                throw new File_Exception('Error writing data to file.');
            }

            break;

        case 2:
        case 3:
            $dir_exists = file_exists($path);

            if ($dir_exists && $replace) {
                try {
                    File::recursiveDelete($path);
                } catch (File_Exception $e) {
                    throw new File_Exception('Error removing old file.');
                }

                $dir_exists = false;
            }

            if (!$dir_exists) {
                $r_code = mkdir($path);

                if (!$r_code) {
                    throw new File_Exception(
                        'Error creating directory for new package.'
                    );
                }
            }

            if (($file_type_id == 2) && !is_null($data)) {
                if (is_array($data)) {
                    $files_not_moved = array();

                    foreach ($data as $fi) {
                        $r_code = rename(
                            $fi['tmp_name'],
                            $path . '/' . $fi['name']
                        );

                        if (!$r_code) {
                            $files_not_moved[] = $fi['name'];
                        }
                    }

                    if (count($files_not_moved) > 0) {
                        throw new File_Exception(
                            'Error moving files '
                            . join(', ', $files_not_moved)
                        );
                    }
                } else {
                    $za = new MMZipArchive();

                    $r_code = is_resource($data) ? $za->openStream($data)
                        : $za->open($data);

                    if (!$r_code) {
                        throw new File_Exception(
                            'Error opening zip data.',
                            $r_code
                        );
                    }

                    $r_code = $za->extractTo($path);
                    $za->close();
                    $za->scheduleDelete();

                    if (!$r_code) {
                        throw new File_Exception(
                            'Error extracting files from zip.'
                        );
                    }
                }
            }

            break;
        }
    }
}
