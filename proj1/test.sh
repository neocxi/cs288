java -cp assign1.jar:target/scala-2.11/assign1-submit.jar -server -mx2000m edu.berkeley.nlp.assignments.assign1.LanguageModelTester -path data -lmType ${1:-TRIGRAM} # -sanityCheck
