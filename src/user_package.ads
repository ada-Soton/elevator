--  Copyright (c) 2020, 2023 University of Southampton.
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in all
--  copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
--  SOFTWARE.

--  This package defines the type for button types.
--
--  @author: htson
--  @version: 1.0
with Elevator; use Elevator;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package User_Package with SPARK_Mode is

   procedure user with
     Global => (Input => (Default_Width,
                          Default_Base,
                          floor,
                          motor,
                          direction,
                          door),
                In_Out => (File_System,
                          floor_buttons_array,
                          up_buttons_array,
                          down_buttons_array)
               ),
       Pre => Invariants(floor,
                         direction,
                         motor,
                         door,
                         floor_buttons_array,
                         up_buttons_array,
                         down_buttons_array),
     Post => Invariants(floor,
                        direction,
                        motor,
                        door,
                        floor_buttons_array,
                        up_buttons_array,
                        down_buttons_array);
end User_Package;
