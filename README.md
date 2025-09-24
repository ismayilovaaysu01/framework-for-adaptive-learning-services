# framework-for-adaptive-learning-services

# Recommendation Service Prototype (Haskell)

This repository contains a prototype implementation of recommendation services 
for adaptive learning systems, written in Haskell.  
Each file corresponds to an implementation inspired by a specific study in the 
literature. Together, they illustrate how recommendation can be defined and 
instantiated using either **learner models** or **peer-learner models**.  

The code is provided as supplementary material for peer review.  

---

## Files
- **Chen et al.hs**  
  Implements a curriculum sequencing service based on a modified Item Response 
  Theory (IRT) model. Uses a learner model to balance task difficulty and learner ability.  

- **Jagan et al.hs**  
  Implements a learner-behavior-based recommendation using Latent Dirichlet Allocation 
  (LDA) to uncover behavioral patterns. Demonstrates recommendations grounded in a 
  learner model.  

- **Nguyen et al.hs**  
  Implements a collaborative filtering approach for course recommendation. Uses a 
  peer-learner model built on data from multiple students.  

- **Rodriguez et al.hs**  
  Implements a recommendation service for personalized homework based on formative 
  assessment of fraction tasks. Uses learner performance data in a learner model.  

- **README.md**  
  This file. Provides documentation and instructions.  

---

## Requirements
- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)  
- Tested with **GHC 9.x**  

---

## Installation
Clone or download this repository, then build with:

```bash
stack build
