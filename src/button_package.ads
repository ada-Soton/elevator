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
with Floor_Package; use Floor_Package; -- This is to access TOP_FLOOR constant

package Button_Package with SPARK_Mode is
   -- Floor buttons arrays are a Boolean array on the range 0 to TOP_FLOOR.
   type FLOOR_BUTTONS_ARRAY_Type is
     array (Integer range 0 .. TOP_FLOOR) of Boolean;

   -- Up buttons arrays are a Boolean array on the range 0 to TOP_FLOOR - 1.
   type UP_BUTTONS_ARRAY_Type is
     array (Integer range 0 .. TOP_FLOOR - 1) of Boolean;

   -- Down buttons arrays are a Boolean array on the range 1 to TOP_FLOOR.
   type DOWN_BUTTONS_ARRAY_Type is
     array (Integer range 1 .. TOP_FLOOR) of Boolean;
end Button_Package;
