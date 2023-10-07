import rdflib
from rdflib import Graph
from rdflib.namespace import RDF, OWL, RDFS, FOAF, XSD
from rdflib import Literal, BNode, URIRef, Graph, Namespace
import numpy as np
import pandas as pd
import json
from random import random
from tqdm import tqdm
from pprint import pprint
from owlready2 import get_ontology, sync_reasoner
import owlready2


#running HermiT initially:
onto = get_ontology("file://odo_v3_app.owl").load()
with onto:
    sync_reasoner(infer_property_values = True)

onto.save("temp_file.owl")


# Namespace
odo = Namespace("http://www.semanticweb.org/menarizk/ontologies/2023/8/ODO/")

# Initialize graph, and read and parse ontology
g = Graph()


# read in exported owl file from protege
g.parse('temp_file.owl')


# # outcome dependence, predictive need
# for ag1 in g.subjects(RDF.type, odo.Agent, unique=True):
#         for ag2 in g.subjects(RDF.type, odo.Agent, unique=True):
#             if ag1 != ag2:
#
#                 # outcome dependence
#                 for e in g.subjects(RDF.type, odo.Evaluation, unique=True):
#
#                     # two agents case
#                     if ((e, odo.hasEvaluatee, ag1) in g) and ((e, odo.hasEvaluatee, ag2) in g):
#                             d = odo.term(f"OD_{odo.ag1.split('/')[-1]}_{odo.ag2.split('/')[-1]}_{odo.e.split('/')[-1]}")
#                             g.add((d,RDF.type,odo.OutcomeDependency))
#                             g.add((d, odo.hasDepender, ag1))
#                             g.add((d, odo.hasDependee, ag1))
#                             g.add((d, odo.hasEvaluation, ag1))
#
#                     # common membership case
#                     for ag3 in g.subjects(RDF.type, odo.Evaluation, unique=True):
#                         if ((e, odo.hasEvaluatee, ag3) in g) and ((ag1, odo.memberOf, ag3) in g) and ((ag2, odo.memberOf, ag3) in g):
#                             d = odo.term(f"OD_{odo.ag1.split('/')[-1]}_{odo.ag2.split('/')[-1]}_{odo.e.split('/')[-1]}")
#                             g.add((d,RDF.type,odo.OutcomeDependency))
#                             g.add((d, odo.hasDepender, ag1))
#                             g.add((d, odo.hasDependee, ag1))
#                             g.add((d, odo.hasEvaluation, ag1))
#
#                 # predictive need
#                 for act1 in g.subjects(RDF.type, odo.Activity, unique=True):
#                     for act2 in g.subjects(RDF.type, odo.Activity, unique=True):
#                         if ((ag1, odo.performs, act1) in g) and ((ag1, odo.performs, act2) in g) and ((act1, odo.dependsOn, act2) in g):
#                             pn = odo.term(f"PN_{odo.ag1.split('/')[-1]}_{odo.ag2.split('/')[-1]}_{odo.act1.split('/')[-1]}_{odo.act2.split('/')[-1]}")
#                             g.add((pn, RDF.type, odo.PredictiveNeed))
#                             g.add((pn, odo.hasPredicandAgent, ag2))
#                             g.add((pn, odo.hasPredictorAgent, ag1))
#                             g.add((pn, odo.hasPredicandActivity, act2))
#                             g.add((pn, odo.hasPredictorActivity, act1))
#
#                             # epistemic dependence
#                             # for e in g.subjects(RDF.type, odo.Evaluation, unique=True):
#                             #     for i in g.subjects(RDF.type, odo.Incentive, unique=True):
#                             #         if ((e, odo.hasEvaluatee, ag1) in g) and ((e, odo.hasIncentive, i) in g)
#                             #
#                             #
#                             #         for s in g.objects(act1, odo.causes, unique=True):
#                             #             if

# SOLE CONTRIBUTOR TO
sc_query = """
SELECT DISTINCT ?ag1 ?x
WHERE {
    ?ag1 odo:contributorTo ?x.
    FILTER NOT EXISTS {
        ?ag2 odo:contributorTo ?x.
        FILTER (?ag1 != ?ag2).
        }
}
"""
sc_res = g.query(sc_query, initNs={'odo': odo})

if len(sc_res) != 0:
    for row in sc_res:
        if (row.ag1, odo.soleContributorTo, row.x) not in g:
            g.add((row.ag1, odo.soleContributorTo, row.x))

# OUTCOME DEPENDENCY
od1_query = """
SELECT DISTINCT ?ag1 ?ag2 ?e
WHERE {
   ?ag1 rdf:type odo:Agent .
   ?ag2 rdf:type odo:Agent .
   ?e rdf:type odo:Evaluation .

   ?e odo:hasEvaluatee ?ag1 .
   ?e odo:hasEvaluatee ?ag2 .
}
"""

od2_query = """
SELECT DISTINCT ?ag1 ?ag2 ?e
WHERE {
    ?ag1 rdf:type odo:Agent.
    ?ag2 rdf:type odo:Agent .
    FILTER (?ag1 != ?ag2).
    ?e odo:hasEvaluatee ?ag3 .
    ?ag1 odo:memberOf ?ag3 .
    ?ag1 odo:memberOf ?ag3 .
}
"""

od1_res = g.query(od1_query, initNs={'odo': odo})
od2_res = g.query(od2_query, initNs={'odo': odo})
if len(od1_res) != 0:
    print(45)
    for row in od1_res:
        d = odo.term(f"OD_{row.ag1.split('/')[-1]}_{row.ag2.split('/')[-1]}_{row.e.split('/')[-1]}")
        g.add((d, RDF.type, odo.OutcomeDependency))
        g.add((d, odo.hasDepender, row.ag1))
        g.add((d, odo.hasDependee, row.ag2))
        g.add((d, odo.hasEvaluation, row.e))

if len(od2_res) != 0:
    for row in od2_res:
        d = odo.term(f"OD_{row.ag1.split('/')[-1]}_{row.ag2.split('/')[-1]}_{row.e.split('/')[-1]}")
        if (d, RDF.type, odo.OutcomeDependency) not in g:
            g.add((d, RDF.type, odo.OutcomeDependency))
            g.add((d, odo.hasDepender, row.ag1))
            g.add((d, odo.hasDependee, row.ag2))
            g.add((d, odo.hasEvaluation, row.e))

# PREDICTIVE NEED
pn_query = """
SELECT DISTINCT ?ag1 ?ag2 ?act1 ?act2
WHERE {
    ?ag1 odo:performs ?act1.
    ?ag2 odo:performs ?act2.
    ?act1 odo:dependsOn ?act2. 
    FILTER (?ag1 != ?ag2).
}
"""

pn_res =  g.query(pn_query, initNs={'odo': odo})
if len(pn_res) != 0:
    for row in pn_res:
        pn = odo.term(f"PN_{row.ag1.split('/')[-1]}_{row.ag2.split('/')[-1]}_{row.act1.split('/')[-1]}_{row.act2.split('/')[-1]}")
        g.add((pn, RDF.type, odo.PredictiveNeed))
        g.add((pn, odo.hasPredictandAgent, row.ag2))
        g.add((pn, odo.hasPredictandActivity, row.act2))
        g.add((pn, odo.hasPredictorAgent, row.ag1))
        g.add((pn, odo.hasPredictorActivity, row.act1))

# EPISTEMIC DEPENDENCE
ed1_query = """
SELECT DISTINCT ?ag1 ?ag2 ?e
WHERE {
    ?pn odo:hasPredictorAgent ?ag1.
    ?pn odo:hasPredictandAgent ?ag2.
    ?pn odo:hasPredictorActivity ?act1.
    ?e odo:hasEvaluatee ?ag1.
    ?e odo:hasIncentive ?i.
    ?i odo:hasRecipient ?ag1.
    ?e odo:hasSubject ?act1.
}"""

ed2_query = """
SELECT DISTINCT ?ag1 ?ag2 ?e
WHERE {
    ?pn odo:hasPredictorAgent ?ag1.
    ?pn odo:hasPredictandAgent ?ag2.
    ?pn odo:hasPredictorActivity ?act1.
    ?e odo:hasEvaluatee ?ag1.
    ?e odo:hasIncentive ?i.
    ?i odo:hasRecipient ?ag1.
    ?e odo:hasSubject ?s.
    ?act1 odo:causes ?s.
}"""

ed1_res =  g.query(ed1_query, initNs={'odo': odo})
ed2_res =  g.query(ed2_query, initNs={'odo': odo})

if len(ed1_res) != 0:
    for row in ed1_res:
        d = odo.term(f"ED_{row.ag1.split('/')[-1]}_{row.ag2.split('/')[-1]}_{row.e.split('/')[-1]}")
        g.add((d, RDF.type, odo.EpistemicDependency))
        g.add((d, odo.hasDepender, row.ag1))
        g.add((d, odo.hasDependee, row.ag2))
        g.add((d, odo.hasEvaluation, row.e))

if len(ed2_res) != 0:
    for row in ed2_res:
        d = odo.term(f"ED_{row.ag1.split('/')[-1]}_{row.ag2.split('/')[-1]}_{row.e.split('/')[-1]}")
        if (d, RDF.type, odo.EpistemicDependency) not in g:
            g.add((d, RDF.type, odo.EpistemicDependency))
            g.add((d, odo.hasDepender, row.ag1))
            g.add((d, odo.hasDependee, row.ag2))
            g.add((d, odo.hasEvaluation, row.e))

# REWARD DEPENDENCE
rd1_query = """
SELECT DISTINCT ?ag1 ?ag2 ?e
WHERE {
    ?od rdf:type odo:OutcomeDependency.
    ?od odo:hasEvaluation ?e.
    ?od odo:hasDepender ?ag1.
    ?od odo:hasDependee ?ag2.
    ?e odo:hasIncentive ?i.
    ?ag1 odo:recipientOf ?i.
    ?ag2 odo:recipientOf ?i.
}
"""

rd2_query = """
SELECT DISTINCT ?ag1 ?ag2 ?e
WHERE {
    ?ed rdf:type odo:EpistemicDependency.
    ?ed odo:hasDepender ?ag1.
    ?ed odo:hasDependee ?ag2.
    ?e odo:hasIncentive ?i.
    ?ag1 odo:recipientOf ?i.
    ?ag2 odo:recipientOf ?i.
}
"""

rd1_res =  g.query(rd1_query, initNs={'odo': odo})
rd2_res =  g.query(rd2_query, initNs={'odo': odo})

if len(rd1_res) != 0:
    for row in rd1_res:
        d = odo.term(f"RD_{row.ag1.split('/')[-1]}_{row.ag2.split('/')[-1]}_{row.e.split('/')[-1]}")
        g.add((d, RDF.type, odo.RewardDependency))
        g.add((d, odo.hasDepender, row.ag1))
        g.add((d, odo.hasDependee, row.ag2))
        g.add((d, odo.hasEvaluation, row.e))

if len(rd2_res) != 0:
    for row in rd2_res:
        d = odo.term(f"RD_{row.ag1.split('/')[-1]}_{row.ag2.split('/')[-1]}_{row.e.split('/')[-1]}")
        if (d, RDF.type, odo.RewardDependency) not in g:
            g.add((d, RDF.type, odo.RewardDependency))
            g.add((d, odo.hasDepender, row.ag1))
            g.add((d, odo.hasDependee, row.ag2))
            g.add((d, odo.hasEvaluation, row.e))

# COORDINATION REQUIREMENT
coord_req_query = """
SELECT DISTINCT ?ag1 ?ag2
WHERE {
    ?ed rdf:type odo:EpistemicDependency.
    ?ed odo:hasDepender ?ag1.
    ?ed odo:hasDependee ?ag2.
}
"""

coord_req_res =  g.query(coord_req_query, initNs={'odo': odo})
if len(coord_req_res) != 0:
    for row in coord_req_res:
        if (row.ag1, odo.coordinationRequirement, row.ag2) not in g:
            g.add((row.ag1, odo.coordinationRequirement, row.ag2))


# FREE RIDE RISK
frr_query_1 = """
SELECT DISTINCT ?ag1 ?e
WHERE {
    ?od rdf:type odo:OutcomeDependency.
    ?od odo:hasDepender ?ag1.
    ?od odo:hasEvaluation ?e.
    FILTER NOT EXISTS {
        ?e odo:hasSubject ?x.
        ?ag1 odo:soleContributorTo ?x.
        }
}
"""

frr_query_2 = """
SELECT DISTINCT ?ag1 ?e
WHERE {
    ?ag1 odo:performs ?act1.
    ?ag2 odo:performs ?act2.
    ?e odo:hasEvaluatee ?ag1.
    ?e odo:hasEvaluatee ?ag2.
    ?e odo:hasSubject ?act1.
    ?e odo:hasSubject ?act2.
    ?s rdf:type odo:StrategicSubstitutes.
    ?s odo:hasActivity ?act1.
    ?s odo:hasActivity ?act2.
}
"""
ffr_res_1 =  g.query(frr_query_1, initNs={'odo': odo})
ffr_res_2 =  g.query(frr_query_2, initNs={'odo': odo})

if len(ffr_res_1) != 0:
    for row in ffr_res_1:
        if (row.ag1, odo.freeRideRisk, row.e) not in g:
            g.add((row.ag1, odo.freeRideRisk, row.e))

if len(ffr_res_2) != 0:
    for row in ffr_res_2:
        if (row.ag1, odo.freeRideRisk, row.e) not in g:
            g.add((row.ag1, odo.freeRideRisk, row.e))

# SHIRK RISK
sr_query_1 = """
SELECT DISTINCT ?ag1 ?act1
WHERE {
    ?ag1 odo:performs ?act1.
    FILTER NOT EXISTS {
        ?e odo:hasEvaluatee ?ag1.
        ?e odo:hasPerformanceTarget ?pt.
        ?pt odo:performanceSpecificationOf ?act1.
        }
}
"""

sr_query_2 = """
SELECT DISTINCT ?ag1 ?t1
WHERE {
    ?ag1 odo:hasTask ?t1.
    ?t1 odo:towards ?g1.
    ?g1 odo:hasDesiredState ?s1.
    FILTER NOT EXISTS {
        ?e odo:hasEvaluatee ?ag1.
        ?e odo:hasStateTarget ?s1.
        }
}
"""

sr_res_1 =  g.query(sr_query_1, initNs={'odo': odo})
sr_res_2 =  g.query(sr_query_2, initNs={'odo': odo})

if len(sr_res_1) != 0:
    for row in sr_res_1:
        if (row.ag1, odo.shirkRisk, row.act1) not in g:
            g.add((row.ag1, odo.shirkRisk, row.act1))

if len(sr_res_2) != 0:
    for row in sr_res_2:
        if (row.ag1, odo.shirkRisk, row.t1) not in g:
            g.add((row.ag1, odo.shirkRisk, row.t1))

# SUB-GOAL OPTIMIZATION RISK
sgor_query = """SELECT DISTINCT ?ag1 ?ag2 ?s3
WHERE {
    ?g odo:hasDesiredState ?s3.
    ?ag1 odo:performs ?act1.
    ?ag2 odo:performs ?act2.
    ?act1 odo:causes ?s1.
    ?act2 odo:causes ?s2.
    ?sc rdf:type odo:StrategicComplements.
    ?sc odo:hasActivity ?act1.
    ?sc odo:hasActivity ?act2.
    ?sc odo:hasState ?s3.
    ?e1 odo:hasEvaluatee ?ag1.
    ?e2 odo:hasEvaluatee ?ag2.
    ?e1 odo:hasStateTarget ?s1.
    ?e2 odo:hasStateTarget ?s2.
    FILTER NOT EXISTS {
        ?e3 odo:hasStateTarget ?s3
        }
    }
"""

sgor_res =  g.query(sgor_query, initNs={'odo': odo})

if len(sgor_res) != 0:
    for row in sgor_res:
        if (row.ag1, odo.subGoalOptRisk, row.s3) not in g:
            g.add((row.ag1, odo.subGoalOptRisk, row.s3))
        if (row.ag2, odo.subGoalOptRisk, row.s3) not in g:
            g.add((row.ag2, odo.subGoalOptRisk, row.s3))

# COOPERATION RISK
coop_risk_query_1 = """SELECT DISTINCT ?ag1 ?ag2
WHERE {
    ?e odo:hasEvaluatee ?ag1.
    ?e odo:hasEvaluatee ?ag2.
    {
        {?ag1 odo:freeRideRisk ?e.}
        UNION
        {?ag2 odo:freeRideRisk ?e.}
        }
    }
"""

coop_risk_query_2 = """SELECT DISTINCT ?ag1 ?ag2
WHERE {
    ?ag1 odo:memberOf ?ag3.
    ?ag2 odo:memberOf ?ag3.
    ?e odo:hasEvaluatee ?ag3.
    {
        {?ag1 odo:freeRideRisk ?e.}
        UNION
        {?ag2 odo:freeRideRisk ?e.}
        }
    }
"""

coop_risk_query_3 = """SELECT DISTINCT ?ag1 ?ag2
WHERE {
    ?ed rdf:type odo:EpistemicDependency.
    ?ed odo:hasDepender ?ag1.
    ?ed odo:hasDependee ?ag2.
    ?ed odo:hasEvaluation ?e.
    ?e odo:hasSubject ?x.
    ?ag2 odo:shirkRisk ?x.
    }
"""

coop_risk_query_4 = """SELECT DISTINCT ?ag1 ?ag2
WHERE {
    ?ag1 odo:subGoalOptRisk ?s.
    ?ag2 odo:subGoalOptRisk ?s.
    FILTER (?ag1 != ?ag2).
    }
"""

coop_risk_results =  [g.query(coop_risk_query_1, initNs={'odo': odo}),
                      g.query(coop_risk_query_2, initNs={'odo': odo}),
                      g.query(coop_risk_query_3, initNs={'odo': odo}),
                      g.query(coop_risk_query_4, initNs={'odo': odo})]


sr_res_1 =  g.query(sr_query_1, initNs={'odo': odo})
sr_res_2 =  g.query(sr_query_2, initNs={'odo': odo})

for res in coop_risk_results:
    if len(res) !=0:
        for row in res:
            if (row.ag1, odo.cooperationRisk, row.ag2) not in g:
                g.add((row.ag1, odo.cooperationRisk, row.ag2))




# if len(qres) = g.query(ed_query, initNs={'odo': odo})
# print(qres)
# for row in qres:
#     print(f"{row.a1} is dependent on {row.a2}")



                        
g.serialize(destination="odo_processed_test_v2.ttl", format="ttl")



