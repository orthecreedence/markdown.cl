(in-package :markdown.cl-test)
(in-suite markdown.cl-table-test)


(test simple-tables
  (is  (null (tree-diff (tree (parse
                               "Header 1  | Header 2
--------- | ---------
Cell 1    | Cell 2
Cell 3    | Cell 4"                       ))
                        (xmls:parse (concatenate 'string "<html>" "<table>
  <theader>
    <tr><th>Header 1  </th><th> Header 2</th></tr>
  </theader>
  <tbody>
    <tr><td>Cell 1    </td><td> Cell 2</td></tr>
    <tr><td>Cell 3    </td><td> Cell 4</td></tr>
  </tbody>
</table>" "</html>")))))

  (is  (null (tree-diff (tree (parse
                               "| Header 1  | Header 2
| --------- | ---------
| Cell 1    | Cell 2
| Cell 3    | Cell 4"))
                        (xmls:parse (concatenate 'string "<html>"
                                                 "
<table>
  <theader>
    <tr><th> Header 1  </th><th> Header 2</th></tr>
  </theader>
  <tbody>
    <tr><td> Cell 1    </td><td> Cell 2</td></tr>
    <tr><td> Cell 3    </td><td> Cell 4</td></tr>
  </tbody>
</table>
" "</html>")))))

  (is  (null (tree-diff (tree (parse
                               "Header 1  | Header 2  |
--------- | --------- |
Cell 1    | Cell 2    |
Cell 3    | Cell 4    |"))
                        (xmls:parse (concatenate 'string "<html>"
                                                 "<table>
  <theader>
    <tr><th>Header 1  </th><th> Header 2  </th></tr>
  </theader>
  <tbody>
    <tr><td>Cell 1    </td><td> Cell 2    </td></tr>
    <tr><td>Cell 3    </td><td> Cell 4    </td></tr>
  </tbody>
</table>
" "</html>")))))

  (is  (null (tree-diff (tree (parse
                               "| Header 1  | Header 2  |
| --------- | --------- |
| Cell 1    | Cell 2    |
| Cell 3    | Cell 4    |"))
                        (xmls:parse (concatenate 'string "<html>"
                                                 "<table>
  <theader>
    <tr><th> Header 1  </th><th> Header 2  </th></tr>
  </theader>
  <tbody>
    <tr><td> Cell 1    </td><td> Cell 2    </td></tr>
    <tr><td> Cell 3    </td><td> Cell 4    </td></tr>
  </tbody>
</table>
" "</html>"))))))

(test one-column-one-row-tables
  (is  (null (tree-diff (tree (parse
                               "| Header
| -------
| Cell"))
                        (xmls:parse (concatenate 'string "<html>"
                                                 "<table>
  <theader>
    <tr><th> Header</th></tr>
  </theader>
  <tbody>
    <tr><td> Cell</td></tr>
  </tbody>
</table>
" "</html>")))))

  (is  (null (tree-diff (tree (parse
                               "Header  |
------- |
Cell    |
"))
                        (xmls:parse (concatenate 'string "<html>"
                                                 "<table>
  <theader>
    <tr><th>Header  </th></tr>
  </theader>
  <tbody>
    <tr><td>Cell    </td></tr>
  </tbody>
</table>
" "</html>")))))

  (is  (null (tree-diff (tree (parse
                               "| Header  |
| ------- |
| Cell    |
"))
                        (xmls:parse (concatenate 'string "<html>"
                                                 "<table>
  <theader>
    <tr><th>Header  </th></tr>
  </theader>
  <tbody>
    <tr><td>Cell    </td></tr>
  </tbody>
</table>
" "</html>"))))))

(test table-alignment
  (is  (null (tree-diff (tree (parse
                               "| Default   | Left     |  Center   |     Right  |
| --------- |:--------- |:---------:| ---------:|
| Long Cell | Long Cell | Long Cell | Long Cell |
| Cell      | Cell      |   Cell    |     Cell  |"))
                        (xmls:parse (concatenate 'string "<html>"
                                                 "<table>
  <theader>
    <tr><th> Default   </th><th align=\"left\"> Left     </th><th align=\"center\">  Center   </th><th align=\"right\">     Right  </th></tr>
  </theader>
  <tbody>
    <tr><td> Long Cell </td><td> Long Cell </td><td> Long Cell </td><td> Long Cell </td></tr>
    <tr><td> Cell      </td><td> Cell      </td><td>   Cell    </td><td>     Cell  </td></tr>
  </tbody>
</table>" "</html>")))))
  (is  (null (tree-diff (tree (parse
                               "| Default   | Left     |  Center   |     Right  |
| --------- | :--------- | :---------: | ---------: |
| Long Cell | Long Cell | Long Cell | Long Cell |
| Cell      | Cell      |   Cell    |     Cell  |"))
                        (xmls:parse (concatenate 'string "<html>"
                                                 "<table>
  <theader>
    <tr><th> Default   </th><th align=\"left\"> Left     </th><th align=\"center\">  Center   </th><th align=\"right\">     Right  </th></tr>
  </theader>
  <tbody>
    <tr><td> Long Cell </td><td> Long Cell </td><td> Long Cell </td><td> Long Cell </td></tr>
    <tr><td> Cell      </td><td> Cell      </td><td>   Cell    </td><td>     Cell  </td></tr>
  </tbody>
</table>" "</html>"))))))

(test empty-cells
  (is  (null (tree-diff (tree (parse
                               "| Header 1  | Header 2  |
| --------- | --------- |
| A         | B         |
| C         |           |"))
                        (xmls:parse (concatenate 'string "<html>"
                                                 "<table>
  <theader>
    <tr><th> Header 1  </th><th> Header 2  </th></tr>
  </theader>
  <tbody>
    <tr><td> A         </td><td> B         </td></tr>
    <tr><td> C         </td><td>           </td></tr>
  </tbody>
</table>" "</html>")))))

  (is  (null (tree-diff (tree (parse
                               "| Header 1  | Header 2  |
| --------- | --------- |
| A         | B         |
|           | D          |"))
                        (xmls:parse (concatenate 'string "<html>"
                                                 "<table>
  <theader>
    <tr><th> Header 1  </th><th> Header 2  </th></tr>
  </theader>
  <tbody>
    <tr><td> A         </td><td> B         </td></tr>
    <tr><td>           </td><td> D          </td></tr>
  </tbody>
</table>" "</html>"))))))

(test missing-tailing-pipe
  (is  (null (tree-diff (tree (parse
                               "Header 1  | Header 2
--------- | --------- |
Cell      | Cell      |
Cell      | Cell      |

Header 1  | Header 2  |
--------- | --------- 
Cell      | Cell      |
Cell      | Cell      |

Header 1  | Header 2  |
--------- | --------- |
Cell      | Cell    
Cell      | Cell      |

Header 1  | Header 2  |
--------- | --------- |
Cell      | Cell      |
Cell      | Cell    "))
                        (xmls:parse (concatenate 'string "<html>"
                                                 "<table>
  <theader>
    <tr><th>Header 1  </th><th> Header 2</th></tr>
  </theader>
  <tbody>
    <tr><td>Cell      </td><td> Cell      </td></tr>
    <tr><td>Cell      </td><td> Cell      </td></tr>
  </tbody>
</table>


<table>
  <theader>
    <tr><th>Header 1  </th><th> Header 2  </th></tr>
  </theader>
  <tbody>
    <tr><td>Cell      </td><td> Cell      </td></tr>
    <tr><td>Cell      </td><td> Cell      </td></tr>
  </tbody>
</table>


<table>
  <theader>
    <tr><th>Header 1  </th><th> Header 2  </th></tr>
  </theader>
  <tbody>
    <tr><td>Cell      </td><td> Cell    </td></tr>
    <tr><td>Cell      </td><td> Cell      </td></tr>
  </tbody>
</table>


<table>
  <theader>
    <tr><th>Header 1  </th><th> Header 2  </th></tr>
  </theader>
  <tbody>
    <tr><td>Cell      </td><td> Cell      </td></tr>
    <tr><td>Cell      </td><td> Cell    </td></tr>
  </tbody>
</table>" "</html>"))))))

