function uniq_
   set -e uniq__orig_array_
   for orig_item in $argv
      for item in uniq__orig_array_
          if string match --quiet --all --invert $item $orig_item
              set uniq__orig_array_ $uniq__orig_array_ $orig_item 
          end
      end
   end

   for item in $uniq__orig_array_
       echo $item
   end
end
    
