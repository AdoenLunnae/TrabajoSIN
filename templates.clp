(deftemplate disease
    (slot name (type SYMBOL)(default ?NONE))
    (multislot symptoms)
    (slot numconfirmed (type NUMBER)(default 0))
    (slot probability (type FLOAT)(default 0))