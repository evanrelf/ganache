module Ganache.Spec where

import Data.Text (Text)
import Data.Word (Word8)

data Field = Field
  { name :: !Text
  , size :: !Word8
  , kind :: !FieldKind
  , requirement :: !Requirement
  }

data FieldKind
  = Constant !Text
  | Other

data Requirement
  = Optional
  | Required
  | Mandatory

fileHeaderRecord :: [Field]
fileHeaderRecord =
  [ Field{ name = "Record Type Code",                 size =  1, kind = Constant "1",   requirement = Mandatory }
  , Field{ name = "Priority Code",                    size =  2, kind = Constant "01",  requirement = Required  }
  , Field{ name = "Immediate Destination",            size = 10, kind = Other,          requirement = Mandatory }
  , Field{ name = "Immediate Origin",                 size = 10, kind = Other,          requirement = Mandatory }
  , Field{ name = "File Creation Date",               size =  6, kind = Other,          requirement = Mandatory }
  , Field{ name = "File Creation Time",               size =  4, kind = Other,          requirement = Optional  }
  , Field{ name = "File ID Modifier",                 size =  1, kind = Other,          requirement = Mandatory }
  , Field{ name = "Record Size",                      size =  3, kind = Constant "094", requirement = Mandatory }
  , Field{ name = "Blocking Factor",                  size =  2, kind = Constant "10",  requirement = Mandatory }
  , Field{ name = "Format Code",                      size =  1, kind = Constant "1",   requirement = Mandatory }
  , Field{ name = "Immediate Destination Name",       size = 23, kind = Other,          requirement = Mandatory }
  , Field{ name = "Immediate Origin or Company Name", size = 23, kind = Other,          requirement = Mandatory }
  , Field{ name = "Reference Code",                   size =  8, kind = Other,          requirement = Optional  }
  ]
