# Generating TPN's

PAMELA has support for generating TPN's.

The following assumes that the `bin` directory has been added to your `PATH`.

Given a PAMELA source file with a TPN such as [choice-tpn.pamela](../test/pamela/choice-tpn.pamela) you can generate the TPN as JSON with this command:

````
pamela -i test/pamela/choice.pamela -c main:tpn:simple-choice-tpn -o choice.tpn.json tpn
````

For a more complete example see [tpn-demo.pamela](../test/pamela/tpn-demo.pamela)

# TPN Structure
The generated EDN file representation of a TPN includes the following structure:

````
;;; In the map snippet below, the values are 
;;; ACTIVITY - which is displayed as an arc in PlanViz
{:act-12                   ;;UID of the activity
 {
  :uid          :act-12,   ;;UID of the activity
  :tpn-type     :activity, ;;Either :activity, :null-activity, or :delay-activity
  :end-node     :node-1,   ;;The node this activity ends in
  :constraints  #{:tc-11}, ;;A set of applicable constraints for this activity
                           ;; If not specified:
                           ;; temporal bounds are assumed to be [0 :infinity], 
                           ;; cost and reward 0

  :htn-node     :hem-15,   ;;Link back to HTN Expanded Method that 
                           ;; generated this activity
  :controllable false,     ;;Needed by planner (true | false)

  :command      "option",  ;;Needed for plant. Deprecated; Use :name instead.      
                           ;;Assume :name value will be same as :command value
  :name         "option",  ;;Represents method name in pamela without args.
  :display-name "Option"   ;;This is the primary string that is displayed by
                           ;; Planviz for this activity (displayed as an arc)
  :args         ["west" 30], ;;Ordered vector of the arguments to the activity
  :argsmap      {"direction" "west", "altitude" 30}, 
                           ;; :argsmap is **OBSOLETE**
  :args-mapping [["direction" "west"] ["altitude" 30]},
                ;; :args-mapping is an ordered vector of pairs
                ;; The first element of each pair is the formal argument name
                ;; The second element of each pair is the argument value
  :plant-id     "p1"       ;;Unique ID for this plant instance
  :plant-part   "part1"    ;;Unique part name for a distinguishable sub-component 
                           ;; of the :plant-id   
  :interface    "RMQ"      ;;The interface used to communicate with the plant instances         
  :label        "TEMP-label", ;;This will be used for Pamela labels
  }
  
;;; NODE - which is displayed as circle in PlanViz 
 :node-7
 {:tpn-type      :state,    ;;Either :state, :c-start, :c-end, :p-start, :p-end
  :uid           :node-7,   ;;UID of the node
  :constraints   #{},       ;;A set of applicable constraints for this
  :activities    #{:act-6}, ;For :state, 1 outgoing activity only
  :incidence-set #{:na-8},
  :htn-node      :hem-12,   ;;Link back to HTN Expanded Method that 
                            ;; generated this activity
  :end-node :node-4},       ;;For :state nodes:
                            ;;Only the 1st node of a sequence points to last
                            ;; node of sequence
                            ;;Other :state nodes won't have an :end-node
                            ;;For :c|p-start nodes: UID of :c|p-end node
 }
  
}
````

### Acknowledgement and Disclaimer:
This material is based upon work supported by the Army Contracting
and DARPA under contract No. W911NF-15-C-0005.
Any opinions, findings and conclusions or recommendations expressed
in this material are those of the author(s) and do necessarily reflect the
views of the Army Contracting Command and DARPA.
