import matplotlib.pyplot as plt

# Sample data
areas = ['Computer\nScience', 'Data\nVisualization', 'Mathematics', 'Statistics', 'Machine\nLearning', 'Domain\nExpertise', 'Communication and\npresentation skills']
skill_level = [60, 40, 80, 50, 30, 90, 50]

fig, ax = plt.subplots(figsize = (8,3), dpi = 300)

# Create a bar graph
ax.bar(areas, skill_level)

size = 10
# Add title and labels
ax.set_title('Data Scientist Profile (Current)', fontsize = size)
ax.set_xlabel('Areas of skills', fontsize = size)
ax.set_ylabel('Skill level', fontsize = size)

plt.xticks(fontsize=7)
plt.yticks(fontsize=size)
# Show the graph
plt.show()

areas = ['Computer\nScience', 'Data\nVisualization', 'Mathematics', 'Statistics', 'Machine\nLearning', 'Domain\nExpertise', 'Communication and\npresentation skills']
skill_level = [70, 90, 85, 70, 80, 80, 60]

fig, ax = plt.subplots(figsize = (8,3), dpi = 300)

# Create a bar graph
ax.bar(areas, skill_level)


# Add title and labels
ax.set_title('Data Scientist Profile (Projected)', fontsize = size)
ax.set_xlabel('Areas of skills', fontsize = size)
ax.set_ylabel('Skill level', fontsize = size)

plt.xticks(fontsize=7)
plt.yticks(fontsize=size)
# Show the graph
plt.show()