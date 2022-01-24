package Lista12

import java.util.concurrent.atomic.AtomicInteger

class MySemaphore(val permits: Int = 1):
  private val atomInt: AtomicInteger = new AtomicInteger(permits)
  

  def acquire(num: Int = 1) =
    while (atomInt.get() <= 0){}
    atomInt.getAndAdd(-num)

  def release(num: Int = 1) =
    atomInt.getAndAdd(num)




