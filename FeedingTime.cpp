//https://app.codesignal.com/interview-practice/task/bdatSZvuKFyjJ8eYw

class Dfs {
private :
	std::unordered_map<int, int> colors;
	std::vector<std::vector<int>*>* graph;
	std::unordered_map<int,int> used;
	std::vector<int> temp;
	int node,starting,cnt;
public :int maximal;

	
public : 
	std::unordered_set<int> visited;
private:

	void dfs_count(int node) {
		auto ns = (*graph)[node];

		for (auto n : *ns) {
			if (!visited.count(n)) {
				temp.push_back(n);
				visited.insert(n);
				dfs_count(n);
			}
		}
	}

public:
	Dfs(int node, std::vector<std::vector<int>*>* graph) : node{ node }, graph{ graph } { maximal = -1; }
private:
	void dfs(std::vector<int>::iterator node) {
		if (maximal != -1 && used.size() >= maximal)
			return;

		if (node == temp.end()) {
			int r = used.size();
			maximal = maximal == -1 ? r : std::min(maximal, r);
			return;
		}
		std::unordered_set<int> colors_left({ 1,2,3,4,5 });
		auto ns = (*graph)[*node];

		for (auto n : *ns) {
			if (colors.count(n)) {
				colors_left.erase(colors[n]);
			}
			
		}

		for (auto color : colors_left) {
			colors[*node] = color;
			int c = used.count(color) == 1 ? used[color] : 0;
			used[color] = c + 1;
			std::vector<int>::iterator nd2 = (node + 1);
			dfs(nd2);
			used[color] = used[color] - 1;
			if (used[color] == 0) {
				used.erase(color);
			}
			colors.erase(*node);
		}

	}
public:
	int DFS(int n) {
		starting = visited.size();
		visited.insert(n);	
		temp.push_back(n);
		dfs_count(n);
		cnt = visited.size() - starting;
		dfs(temp.begin());
		colors.clear();
		used.clear();
		int temp2 = maximal;
		temp.clear();
		maximal = -1;
		return temp2;
	}
};


int feedingTime(std::vector<std::vector<std::string>> classes) {
	std::vector<std::vector<int>*> graph;
	const int n = classes.size();
	for (int i = 0; i < n; i++) {
		graph.push_back(new std::vector<int>());
	}

	for (int i = 0; i < n;i++) {
		std::vector<std::string>* node = &classes[i];
		std::unordered_set<std::string> s(node->begin(), node->end());

		for (int j = i + 1; j < n; j++) {
			std::vector<std::string>* node2 = &classes[j];
			for (auto c : *node2) {
				if (s.count(c)) {
					graph[i]->push_back(j);
					graph[j]->push_back(i);
					break;
				}
			}
		}
	}

	int result = -1;
	Dfs d(0, &graph);
	for (int i = 0; i < (int)graph.size(); i++) {
		if (d.visited.count(i))
			continue;
		int r = d.DFS(i);
		if (r == -1)
			return -1;
		result = r == -1 ? result : std::max(result, r);
		if (d.visited.size() == classes.size())
			return result;
	}

	return result;

}
