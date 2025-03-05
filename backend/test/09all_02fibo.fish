/* Compute the smallest Fibonacci number >= 'limit'
 * For limit = 1000, this is 1597, which we subtract 1500 from to fit in [0, 255]
 * Chris Jeris */
{
  limit = 1000;
  x = 0;
  y = 1;
  while (y < limit) {
    t = x + y;
    x = y;
    y = t;
  }
  return (y - 1500);
}
