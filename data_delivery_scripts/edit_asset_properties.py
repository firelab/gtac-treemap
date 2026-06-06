'''
Updates properties of images in a GEE image collection, consistently with latest Published Catalog Best Practices.

Instructions:
1. Manually edit the `props_to_update` and `props_to_delete` dictionaries to specify which properties to update (with new values) and which to delete (set to None).
2. Run the script. It will prompt for confirmation (y/N) before applying changes to the specified asset.

IMPORTANT: FLDSZCD and FORTYPCD names, values and palette were updated using \GTAC-LCMS-Viewer\src\data\geojson\forest_type_palette_lookup_20250708.js

Prepared by MO Borja (Last update: 06/05/2026)
'''

#%%
import ee
import json
import time
import sys

#%%
# 1. Initialize with TreeMap GCP project
ee.Initialize(project='treemap-386222')

#%%
# 2. Avoid indefinite hangs on network/API calls and allow automatic retries.
ee.data.setDeadline(120000)  # 120 seconds per request
ee.data.setMaxRetries(10)

#%%
# 3. Define asset path
asset_id = 'projects/treemap-386222/assets/2026_UPDATE/2022/TreeMap2022/TreeMap2022_CONUS'
print(f"Updating Asset: {asset_id}...")

#%%
# 4. *** Manually edit this section*** to define which properties to update and/or delete.
'''
Behavior:
- If props_to_update is defined and non-empty: only "True" keys are uploaded.
- If props_to_update is defined and empty: upload skipped.
- If `props_to_delete` is defined and empty, no properties will will be deleted.
- Deletions from props_to_delete run before uploads.
'''
props_to_update = {
     'FLDSZCD_class_names': True,
     'FLDSZCD_class_palette': True,
     'FLDSZCD_class_values': True,

     'FLDTYPCD_class_names': True,
     'FLDTYPCD_class_palette': True,
     'FLDTYPCD_class_values': True,

     'FORTYPCD_class_names': True,
     'FORTYPCD_class_palette': True,
     'FORTYPCD_class_values': True,

     'STDSZCD_class_names': True,
     'STDSZCD_class_palette': True,
     'STDSZCD_class_values': True,
}

# Choose properties to delete by uncommenting below (set value to None).
props_to_delete = {
    #'FLDSZCD_class_names': None,
    # 'FLDSZCD_class_palette': None,
    # 'FLDSZCD_class_values': None,

    # 'FLDTYPCD_class_names': None,
    # 'FLDTYPCD_class_palette': None,
    # 'FLDTYPCD_class_values': None,

    # 'FORTYPCD_class_names': None,
    #'FORTYPCD_class_palette': None,
    # 'FORTYPCD_class_values': None,

    # 'STDSZCD_class_names': None,
    # 'STDSZCD_class_palette': None,
    # 'STDSZCD_class_values': None,
}

#%%
# 5. Create Dictionaries for the properties values.
properties_to_set = {
'FLDSZCD_class_names': [
    'Nonstocked',
    '≤4.9 inches (seedlings/saplings)',
    '5.0-8.9 inches (softwoods)/ 5.0-10.9 inches (hardwoods)',
    '9.0-19.9 inches (softwoods)/ 11.0-19.9 inches (hardwoods)',
    '20.0-39.9 inches',
    '40.0+ inches'
],


#'FLDSZCD_class_names':[
#      "Nonstocked - Meeting the definition of accessible land and one of the following applies (1) less than 10 percent stocked by trees, seedlings, and saplings and not classified as cover trees, or (2) for several woodland species where stocking standards are not available, less than 10 percent canopy cover of trees, seedlings, and saplings.",
#      "≤4.9 inches (seedlings/saplings). At least 10 percent stocking (or 10 percent canopy cover if stocking standards are not available) in trees, seedlings, and saplings, and at least 2/3 of the canopy cover is in trees less than 5.0 inches d.b.h./d.r.c.",
#      "5.0-8.9 inches (softwoods)/ 5.0-10.9 inches (hardwoods). At least 10 percent stocking (or 10 percent canopy cover if stocking standards are not available) in trees, seedlings, and saplings; and at least one-third of the canopy cover is in trees greater than 5.0 inches d.b.h./d.r.c. and the plurality of the canopy cover is in softwoods 5.0-8.9 inches diameter and/or hardwoods 5.0-10.9 inches d.b.h., and/or woodland trees 5.0-8.9 inches d.r.c.",
#      "9.0-19.9 inches (softwoods)/ 11.0-19.9 inches (hardwoods). At least 10 percent stocking (or 10 percent canopy cover if stocking standards are not available) in trees, seedlings, and sapling; and at least one-third of the canopy cover is in trees greater than 5.0 inches d.b.h./d.r.c. and the plurality of the canopy cover is in softwoods 9.0-19.9 inches diameter and/or hardwoods between 11.0-19.9 inches d.b.h., and/or woodland trees 9.0-19.9 inches d.r.c.",
#      "20.0-39.9 inches. At least 10 percent stocking (or 10 percent canopy cover if stocking standards are not available) in trees, seedlings, and saplings; and at least one-third of the canopy cover is in trees greater than 5.0 inches d.b.h./d.r.c. and the plurality of the canopy cover is in trees 20.0-39.9 inches d.b.h.",
#      "40.0+ inches. At least 10 percent stocking (or 10 percent canopy cover if stocking standards are not available) in trees, seedlings, and saplings; and at least one-third of the canopy cover is in trees greater than 5.0 inches d.b.h./d.r.c. and the plurality of the canopy cover is in trees greater than or equal to 40.0 inches d.b.h."
#    ],

#updated
'FLDSZCD_class_palette': [
    'C62363',
    'FEBA12',
    'FFFF00',
    '38A800',
    '73DFFF',
    '5C09FC'
],

'FLDSZCD_class_values': list(range(0, 6)),

# From \GTAC-LCMS-Viewer\src\data\geojson\forest_type_palette_lookup_20250708.js
'FLDTYPCD_class_names': [
    'White / red / jack pine group', 'Jack pine', 'Red pine', 'Eastern white pine', 'Eastern white pine / eastern hemlock', 'Eastern hemlock', 'Spruce / fir group', 'Balsam fir', 'White spruce', 'Red spruce', 'Red spruce / balsam fir', 'Black spruce', 'Tamarack', 'Northern white-cedar', 'Fraser fir', 'Red spruce / Fraser fir', 'Longleaf / slash pine group', 'Longleaf pine', 'Slash pine', 'Tropical softwoods group', 'Tropical pines', 'Loblolly / shortleaf pine group', 'Loblolly pine', 'Shortleaf pine', 'Virginia pine', 'Sand pine', 'Table Mountain pine', 'Pond pine', 'Pitch pine', 'Spruce pine', 'Other eastern softwoods group', 'Eastern redcedar', 'Florida softwoods', 'Pinyon / juniper group', 'Rocky Mountain juniper', 'Juniper woodland', 'Pinyon / juniper woodland', 'Douglas-fir group', 'Douglas-fir', 'Port-Orford-cedar', 'Bigcone Douglas-fir', 'Ponderosa pine group', 'Ponderosa pine', 'Incense-cedar', 'Sugar pine', 'Jeffrey pine', 'Coulter pine', 'Western white pine group', 'Western white pine', 'Fir / spruce / mountain hemlock group', 'White fir', 'Red fir', 'Noble fir', 'Pacific silver fir', 'Engelmann spruce', 'Engelmann spruce / subalpine fir', 'Grand fir', 'Subalpine fir', 'Blue spruce', 'Mountain hemlock', 'Alaska-yellow-cedar', 'Lodgepole pine group', 'Lodgepole pine', 'Hemlock / Sitka spruce group', 'Western hemlock', 'Western redcedar', 'Sitka spruce', 'Western larch group', 'Western larch', 'Redwood group', 'Redwood', 'Giant sequoia', 'Other western softwoods group', 'Knobcone pine', 'Southwestern white pine', 'Bishop pine', 'Monterey pine', 'Foxtail pine / bristlecone pine', 'Limber pine', 'Whitebark pine', 'Miscellaneous western softwoods', 'Western juniper', 'California mixed conifer group', 'California mixed conifer', 'Exotic softwoods group', 'Scotch pine', 'Other exotic softwoods', 'Norway spruce', 'Introduced larch', 'Other softwoods group', 'Other softwoods', 'Oak / pine group', 'Eastern white pine / northern red oak / white ash', 'Eastern redcedar / hardwood', 'Longleaf pine / oak', 'Shortleaf pine / oak', 'Virginia pine / southern red oak', 'Loblolly pine / hardwood', 'Slash pine / hardwood', 'Other pine / hardwood', 'Oak / hickory group', 'Post oak / blackjack oak', 'Chestnut oak', 'White oak / red oak / hickory', 'White oak', 'Northern red oak', 'Yellow-poplar / white oak / northern red oak', 'Sassafras / persimmon', 'Sweetgum / yellow-poplar', 'Bur oak', 'Scarlet oak', 'Yellow-poplar', 'Black walnut', 'Black locust', 'Southern scrub oak', 'Chestnut oak / black oak / scarlet oak', 'Cherry / white ash / yellow-poplar', 'Elm / ash / black locust', 'Red maple / oak', 'Mixed upland hardwoods', 'Oak / gum / cypress group', 'Swamp chestnut oak / cherrybark oak', 'Sweetgum / numeric oak / willow oak', 'Overcup oak / water hickory', 'Atlantic white-cedar', 'Baldcypress / water tupelo', 'Sweetbay / swamp tupelo / red maple', 'Baldcypress / pondcypress', 'Elm / ash / cottonwood group', 'Black ash / American elm / red maple', 'River birch / sycamore', 'Cottonwood', 'Willow', 'Sycamore / pecan / American elm', 'Sugarberry / hackberry / elm / green ash', 'Silver maple / American elm', 'Red maple / lowland', 'Cottonwood / willow', 'Oregon ash', 'Maple / beech / birch group', 'Sugar maple / beech / yellow birch', 'Black cherry', 'Hard maple / basswood', 'Red maple / upland', 'Aspen / birch group', 'Aspen', 'Paper birch', 'Gray birch', 'Balsam poplar', 'Pin cherry', 'Alder / maple group', 'Red alder', 'Bigleaf maple', 'Western oak group', 'Gray pine', 'California black oak', 'Oregon white oak', 'Blue oak', 'Coast live oak', 'Canyon live oak', 'Interior live oak', 'California white oak (valley oak)', 'Tanoak / laurel group', 'Tanoak', 'California laurel', 'Giant chinkapin', 'Other hardwoods group', 'Pacific madrone', 'Other hardwoods', 'Woodland hardwoods group', 'Deciduous oak woodland', 'Evergreen oak woodland', 'Mesquite woodland', 'Cercocarpus (mountain brush) woodland', 'Intermountain maple woodland', 'Miscellaneous woodland hardwoods', 'Tropical hardwoods group', 'Mangrove', 'Palms', 'Dry forest', 'Moist forest', 'Wet and rain forest', 'Lower montane wet and rain forest', 'Cloud forest', 'Other tropical hardwoods', 'Exotic hardwoods group', 'Paulownia', 'Melaleuca', 'Eucalyptus', 'Other exotic hardwoods', 'Nonstocked'
],

# From \GTAC-LCMS-Viewer\src\data\geojson\forest_type_palette_lookup_20250708.js
'FLDTYPCD_class_palette': [
    '94d0aa', '81b0b6', '7f6a37', 'c57f69', 'eec289', 'f08bf0', '51a0b1', 'f3b035', 'eba9d1', 'af8de6', 'bbb92d', 'e48fc8', 'eea97f', 'eaf5e0', 'd2d59b', 'dcc2c0', 'c7f0aa', 'e51eeb', '3a1591', 'c26da6', '67c6ca', '95ef79', 'd5c7e9', 'bc916f', 'd1dfd6', '49f6ae', '7b677b', 'f16791', 'dbf39c', 'dad9bd', '7d2f2e', '45e8d5', '9bc5e5', '4debbe', '3a6a8b', '68f0ec', 'aaa7e7', '4589d2', 'f4df8e', 'f7d969', 'dfa098', 'd8ecf0', 'caf466', 'e64393', '8ce7ef', 'eec7ea', 'dfa9ef', 'b7f2e0', 'bbbcc4', 'b2b1c8', 'adf6d0', 'e2c8f9', 'bae3f5', '8391a7', '9a2d79', 'b4cfa7', '93dbf2', 'baaea2', '8ff390', 'b2fa73', 'c6295f', 'ea6375', 'a1e073', '2db8ab', 'ae73b2', 'e38bae', 'cfb0fa', 'd3a9b3', '90e5d5', 'f12c43', 'b2c387', '8fa42c', 'cde353', '7680ac', '46d7ed', 'eecc39', 'd7baf5', '9a29f0', 'a7c6d7', 'f28a8d', 'ebf385', 'a0cd4f', '81b180', 'cba1fb', '6bc35e', 'f5a3ad', 'd4ddef', 'a049b7', 'e1a0b6', 'f06748', '7a9252', '66d727', 'e645dc', 'aa8d8b', 'ee405d', 'cfadd4', '9190e6', '4e655f', '57adf1', 'b2eb31', 'c6e377', 'bea664', 'c26262', '4b9a91', '72f4b9', 'da90e3', '3ebf45', 'e973bf', 'afe7b3', '81629f', '9144dd', 'e4c0a1', 'ac82a2', 'f5a8e1', 'b486f5', 'ecbf68', 'f340af', 'c0caf5', 'c1f89a', 'd3f3d2', 'ebebc0', 'd350f4', 'd1cf7c', '71f943', '9e87c0', 'b6707a', 'f7e6d7', '8caaf2', 'f1f2f3', 'a3d382', '4ebf86', '8cf45a', 'f4f3b7', 'a1cfc9', 'f0dc9f', '5971ef', '699fcb', 'f7a1f0', '5e62f8', 'f0cdbd', 'a7902b', 'f2f875', 'e6a55a', 'f068e4', 'de55bf', 'cff3c0', 'f7c1f3', '90896c', 'f5beb3', 'b49ec7', '6bc3f5', '6226e8', '7aebcd', 'd2ab20', '515891', 'e47a65', '97b45a', 'cbd14d', '64c2a2', 'adf2ad', 'deefb4', '52b8dc', 'e1f855', '8fa792', 'ebe545', 'c676df', '489744', 'e2f231', 'cfbc8f', '51dc89', 'aa511d', '846ac6', 'bb8fd3', 'f1bcd8', '4f45a6', 'c6f5f4', 'e5dd69', 'c0a8e1', '7f5dd7', 'dfd3d7', '56f277', '28703c', '86e799', 'b9d1bf', 'a5baee', 'f28e2e', 'e7dbed', '5d84e4', 'bb68f3', 'd96f9b', 'efcddd'
],


# From \GTAC-LCMS-Viewer\src\data\geojson\forest_type_palette_lookup_20250708.js
'FLDTYPCD_class_values': [
    '100', '101', '102', '103', '104', '105', '120', '121', '122', '123', '124', '125', '126', '127', '128', '129', '140', '141', '142', '150', '151', '160', '161', '162', '163', '164', '165', '166', '167', '168', '170', '171', '172', '180', '182', '184', '185', '200', '201', '202', '203', '220', '221', '222', '224', '225', '226', '240', '241', '260', '261', '262', '263', '264', '265', '266', '267', '268', '269', '270', '271', '280', '281', '300', '301', '304', '305', '320', '321', '340', '341', '342', '360', '361', '362', '363', '364', '365', '366', '367', '368', '369', '370', '371', '380', '381', '383', '384', '385', '390', '391', '400', '401', '402', '403', '404', '405', '406', '407', '409', '500', '501', '502', '503', '504', '505', '506', '507', '508', '509', '510', '511', '512', '513', '514', '515', '516', '517', '519', '520', '600', '601', '602', '605', '606', '607', '608', '609', '700', '701', '702', '703', '704', '705', '706', '707', '708', '709', '722', '800', '801', '802', '805', '809', '900', '901', '902', '903', '904', '905', '910', '911', '912', '920', '921', '922', '923', '924', '931', '933', '934', '935', '940', '941', '942', '943', '960', '961', '962', '970', '971', '972', '973', '974', '975', '976', '980', '982', '983', '984', '985', '986', '987', '988', '989', '990', '991', '992', '993', '995', '999'
],
'FORTYPCD_class_names': None,
'FORTYPCD_class_palette': None,
'FORTYPCD_class_values': None,
'STDSZCD_class_names': [
    'Large diameter',
    'Medium diameter',  
    'Small diameter',
    'Nonstocked'
],
'STDSZCD_class_palette': [
    '38A800',
    'FFFF00',
    'FEBA12',
    'C62363'
],
'STDSZCD_class_values': [
    '1', 
    '2', 
    '3', 
    '5'
]
}

# Prepare Final Properties Object
if properties_to_set.get('FORTYPCD_class_names') is None:
    properties_to_set['FORTYPCD_class_names'] = properties_to_set.get('FLDTYPCD_class_names')
if properties_to_set.get('FORTYPCD_class_palette') is None:
    properties_to_set['FORTYPCD_class_palette'] = properties_to_set.get('FLDTYPCD_class_palette')
if properties_to_set.get('FORTYPCD_class_values') is None:
    properties_to_set['FORTYPCD_class_values'] = properties_to_set.get('FLDTYPCD_class_values')

# Validate: properties_to_set must not contain None values.
none_keys = [k for k, v in properties_to_set.items() if v is None]
if none_keys:
    print("ERROR: The following keys in `properties_to_set` are None:", none_keys)
    print("Delete operations must be specified via `props_to_delete`. Aborting.")
    sys.exit(1)


#%%
# 6. 
# Determine which keys to include
if 'props_to_update' not in globals():
    print("ERROR: `props_to_update` must be defined at the top of the script (use an empty dict to update nothing). Aborting.")
    sys.exit(1)

if props_to_update:
    update_keys = [k for k, flag in props_to_update.items() if flag]
    print(f"Updating only keys: {update_keys}")
else:
    # Explicit empty dict: user intends to update nothing
    update_keys = []
    print("No keys selected for update; upload step will be skipped.")

# Build upload dict by JSON-encoding non-None values
props_to_upload = {}
for k, v in properties_to_set.items():
    if v is None:
        # skip None here; use props_to_delete if you want to remove keys
        continue
    # If update_keys is set, only include those keys
    if update_keys is not None and k not in update_keys:
        continue
    try:
        # Convert all property values into comma-separated strings:
        # - lists/tuples -> comma-joined items
        # - dicts -> comma-joined "key:val" pairs
        # - scalars -> string representation
        if isinstance(v, (list, tuple)):
            props_to_upload[k] = ','.join(map(str, v))
        elif isinstance(v, dict):
            props_to_upload[k] = ','.join(f"{kk}:{vv}" for kk, vv in v.items())
        else:
            props_to_upload[k] = str(v)
    except Exception:
        # fallback to string conversion on any unexpected error
        props_to_upload[k] = str(v)

# Prepare the properties to DELETE set in `props_to_delete`.
if 'props_to_delete' not in globals():
    print("ERROR: `props_to_delete` must be defined at the top of the script (use an empty dict to delete none). Aborting.")
    sys.exit(1)

# Use the user-provided deletion map directly; an empty dict means delete nothing.
runtime_props_to_delete = props_to_delete



#%%
# 7. Execution 
try:
    # Confirm with the user before making changes
    confirm = input(f"Proceed to apply changes to asset {asset_id}? [y/N]: ")
    if confirm.lower() != 'y':
        print("Aborted by user. No changes applied.")
        sys.exit(0)

    # First, delete any keys explicitly set to None
    if runtime_props_to_delete:
        print(f"Deleting properties: {list(runtime_props_to_delete.keys())}")
        ee.data.setAssetProperties(asset_id, runtime_props_to_delete)
        # small pause to allow EE to process deletions
        time.sleep(1)

    # Then set the new JSON-based properties (add/update) if any were selected
    if props_to_upload:
        print(f"Uploading properties: {list(props_to_upload.keys())}")
        ee.data.setAssetProperties(asset_id, props_to_upload)
    else:
        print("No properties selected for upload; skipping upload step.")
    print("SUCCESS! Properties updated ")
    
    # Verification
    final_info = ee.Image(asset_id).getInfo()
    print("New property keys detected:", len(final_info['properties']))
    print("Updated Properties found in Asset:")
    for key in props_to_upload.keys():
        if key in final_info['properties']:
            print(f" - {key}: {final_info['properties'][key][:50]}...")
    
    #test_prop = final_info['properties']['STDSZCD_class_names']
    #print(f"Property Type check: {type(test_prop)}")


except Exception as e:
    print(f"❌ Error: {e}")
# %%




# %%
# Update the year property. 
#try:
#    ee.data.setAssetProperties(asset_id, {'year': '2020'})
#    print("Success: Property 'year' updated to STRING type.")
#except Exception as e:
#    print(f"Failed to update asset: {e}")



