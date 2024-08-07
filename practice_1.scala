object InventorySystem {
  def main(args: Array[String]): Unit = {
    // Sample inventories
    val inventory1: Map[Int, (String, Int, Double)] = 
      Map(
          
      101 -> ("ProductA", 10, 50.0),102 -> ("ProductB", 20, 30.0),
      103 -> ("ProductC", 5, 100.0) 
    )

    val inventory2: Map[Int, (String, Int, Double)] = 
    Map(
      102 -> ("ProductB", 15, 35.0), 104 -> ("ProductD", 8, 20.0)
    )

    // I. Retrieve from inventory1
    val productNames = inventory1.values.map(_._1).toList //Take the first element
    /*It extracts all the product names from the inventory1 map by accessing 
    the first element of each value tuple.*/
    //It transforms the collection of these names into a List
    println(s"Product Names: $productNames")

    // II. Calculate the total value of all products in inventory1
    val totalValue = inventory1.values.map { case (_, qty, price) => qty * price }.sum
    //The underscore _ is used to ignore the first element of the tuple (the product name)
    //=> symbolies what to do with the matched elements.
    //Case - pattern matching
    println(s"Total Value: $$${totalValue}")
    //$$ is interpreted as a single $ in the resulting string

    // III. Check if inventory1 is empty
    val isEmpty = inventory1.isEmpty
    println(s"Is Inventory1 Empty: $isEmpty")

    // IV. Merge inventory1 and inventory2, updating quantities and retaining the highest price
    val mergedInventory = (inventory1.keySet ++ inventory2.keySet).map 
    /*inventory1.keySet and inventory2.keySet return the sets of keys (product IDs)
    from each inventory.*/
    /*++ is the union operator for sets, combining all unique keys from both 
    inventories into a single set.*/
     { id =>
       
      val details1 = inventory1.get(id) //get IDs
      val details2 = inventory2.get(id)
      
      val mergedDetails = (details1, details2) match {
        case (Some((name1, qty1, price1)), Some((_, qty2, price2))) =>
          (name1, qty1 + qty2, Math.max(price1, price2))
        case (Some(details), None) => details
        case (None, Some(details)) => details
        case _ => throw new IllegalStateException("Unexpected case")
      }
      
      id -> mergedDetails
    }.toMap // Converts the iterable of mapped entries into a Scala Map.
    println(s"Merged Inventory: $mergedInventory")

    // V. Check if a product with a specific ID (e.g., 102) exists and print its details
    val productIdToCheck = 102
    inventory1.get(productIdToCheck) match {
      case Some((name, qty, price)) =>
        println(s"Product ID: $productIdToCheck, Name: $name, Quantity: $qty, Price: $$${price}")
      case None =>
        println(s"Product with ID: $productIdToCheck does not exist in Inventory1")
    }
  }
}
