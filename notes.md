

# Notes




## Thoughts

 Should we allow arrays with shapes as well as arrays of arrays? 

 arr :: Array sh1 (Array sh2 a)

 map f arr    => f :: Array sh2 a -> b
              => map f arr :: Array sh1 (Array sh2 a) -> Array sh1 b


 I think, yes.
 For example "Block" should (for tuning) should create an Array of arrays
 (Block can be implemented using Generate and Prj)

 Block should not affix another dimension to the array it "blocks up".

 

 
 

 