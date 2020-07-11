


data IndSelection = AllIndividuals
    | SelectionList [SelectionSpec]
data SelectionSpec = SelectedInd String
    | SelectedPop String

