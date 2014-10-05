define(new Agent with PriorityAndProposalBased.HavingViews with DomainIterating.AllAsOne with SolutionFiltering.andSharing { agent => 

  import views.{ fullMerge => view }

  using(view)
  
  onStart{
    agent.doSomething()
  }
  
  // definition for all negotiations
  afterStart := {
    spamProposal
    await(view.hasMinimumData)
    state = Negotiating
  }
  
  for(neg <- `queen's position`){
  
    onProposalMessage := {
      proposal => respond(if(proposal breaks constraints) Rejected else Accepted)
    }
    
    onAcceptanceMessage := {
      msg => /* do nothing */
    }
    
    onRejectionMessage := {
      case agent.priority > sender.priority => ignore
      case agent.priority < sender.priority => rejection
    }
    
    when(neg receivesAll responses){
      if((neg.currentProposal satisfies view.externalConstraints) && (solutionFilter accepts messages)) 
	   neg.currentProposal is Accepted 
      else neg.currentProposal is Rejected
    }
    
    onAcceptance := { 
      _ => state = Sleeping
    }
    
    onRejection{
      
    }
    
    def keepOrRaise(pNeg: PriorityNegotiation) = {
      val requestByPriority = pNeg get requests andThen (_.orderBy(_.priority).zipWithIndex)
      val (index, req) = requestByPriority.find(_._2.sender === agent.ref).get
      if(index < (scope.size + 1.0) / 2) Raise else Keep
    }
    
    def raisePriority(pNeg: PriorityNegotiation) = {
      val resp = pNeg get responses
    }
    
    def priorityNegotiation(pNeg: PriorityNegotiation) = {
      spam(pNeg.request)
      when(pNeg receivesAll requests, respond( keepOrRaise(pNeg) )) and {
	  case Keep => 
	  case Raise => when(pNeg receivesAll responses, )
      }
    }
    
    define priority negotiation := priorityNegotiation
    when(noMoreProposals, start priority negotiation)(priorityNegotiation)
  }
  
})