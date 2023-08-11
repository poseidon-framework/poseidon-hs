{
  groupDefs = [
    {name = "MyGroup", entities = ["French", "Spanish", "-<Ind1>"]},
    {name = "MyGroup2", entities = ["asda1", "*pakk*"]}
  ],
  fStatSpecs = [
    {
      type = "F3",
      slots = ["French", "Spanish", "Mbuti"],
      ascertainment = Some {
        outgroup = Some "Chimp",
        refgroup = "CEU",
        lofreq = 0.05,
        hifreq = 0.95
      }
    },
    {
      type = "F4",
      slots = ["French", "Spanish", "Mbuti", "Chimp"],
      ascertainment = None {
        outgroup : Optional Text,
        refgroup : Text,
        lofreq : Double,
        hifreq : Double
      }
    }
  ]
}