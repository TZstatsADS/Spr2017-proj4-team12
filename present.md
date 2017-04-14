# Project 4: Who Is Who -- Entity Resolution

### [Project Description](doc/project4_desc.md)

Term: Spring 2017

+ Team #12
+ Projec title: Entity Resolution
+ Team members
	+ Kai Chen (Presenter)
	+ Senyao Han
	+ Kexin Nie
	+ Yini Zhang
	+ Chenyun Zhu
+ Project summary: In our project, we studied entity resolution through two academic papers which introduce two methods. Paper 2 is about using linear svm, with its own unique way of evaluation: comparing the performance of different variables through its average accuracy. For paper 5, we studied author disambiguation using error-driven machine learning with a ranking loss function. The features we used for are cosine similarity between the words and Euclidean distance in Word2vec model. The detailed methods we applied are as follow: Clusterwise Scoring Function as the partition criterion, Error-driven Online Training to generate training examples and Ranking Perceptron as the loss function. 
	
**Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 

Paper2 SVM: Senyao Han, Kexin Nie

Paper5: Kai Chen, Yini Zhang, Chenyun Zhu

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
