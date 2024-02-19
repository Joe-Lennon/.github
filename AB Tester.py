# -*- coding: utf-8 -*-
"""
Created on Mon Feb 19 12:31:55 2024

@author: joeml
"""

import tkinter as tk
from tkinter import ttk, messagebox, filedialog
import statsmodels.api as sm
import pandas as pd

class ABTestApp:
    def __init__(self, root):
        self.root = root
        self.root.title("A/B Test App")

        # Variables to store user input
        self.sample_size_a = tk.IntVar()
        self.proportion_a = tk.DoubleVar()
        self.sample_size_b = tk.IntVar()
        self.proportion_b = tk.DoubleVar()

        # Variables to store result
        self.p_value = tk.StringVar()
        self.p_value_level = tk.StringVar()
        self.z_score = tk.DoubleVar()
        self.significance = tk.StringVar()
        self.test_type = tk.StringVar(value="two-sided")  # Default: two-sided test
        self.significance_threshold = tk.StringVar(value="0.05")  # Default: 95% significance

        # Create and place widgets
        self.create_widgets()

    def create_widgets(self):
        # Labels and Entry widgets for user input
        ttk.Label(self.root, text="Sample Size A:").grid(row=0, column=0, padx=5, pady=5, sticky="w")
        ttk.Entry(self.root, textvariable=self.sample_size_a).grid(row=0, column=1, padx=5, pady=5)

        ttk.Label(self.root, text="Proportion A:").grid(row=1, column=0, padx=5, pady=5, sticky="w")
        ttk.Entry(self.root, textvariable=self.proportion_a).grid(row=1, column=1, padx=5, pady=5)

        ttk.Label(self.root, text="Sample Size B:").grid(row=2, column=0, padx=5, pady=5, sticky="w")
        ttk.Entry(self.root, textvariable=self.sample_size_b).grid(row=2, column=1, padx=5, pady=5)

        ttk.Label(self.root, text="Proportion B:").grid(row=3, column=0, padx=5, pady=5, sticky="w")
        ttk.Entry(self.root, textvariable=self.proportion_b).grid(row=3, column=1, padx=5, pady=5)

        # Radio buttons for test type
        ttk.Label(self.root, text="Test Type:").grid(row=4, column=0, padx=5, pady=5, sticky="w")
        ttk.Radiobutton(self.root, text="Two-Sided", variable=self.test_type, value="two-sided").grid(row=4, column=1, padx=5, pady=5, sticky="w")
        ttk.Radiobutton(self.root, text="One-Sided (Proportion A > Proportion B)", variable=self.test_type, value="one-sided-greater").grid(row=5, column=1, padx=5, pady=5, sticky="w")
        ttk.Radiobutton(self.root, text="One-Sided (Proportion A < Proportion B)", variable=self.test_type, value="one-sided-less").grid(row=6, column=1, padx=5, pady=5, sticky="w")

        # Dropdown for significance threshold
        ttk.Label(self.root, text="Significance Threshold:").grid(row=7, column=0, padx=5, pady=5, sticky="w")
        threshold_dropdown = ttk.Combobox(self.root, textvariable=self.significance_threshold, values=["0.10", "0.05", "0.01"])
        threshold_dropdown.grid(row=7, column=1, padx=5, pady=5, sticky="w")

        # Button to perform A/B test
        ttk.Button(self.root, text="Perform A/B Test", command=self.perform_ab_test).grid(row=8, column=0, columnspan=2, pady=10)

        # Button to export to Excel
        ttk.Button(self.root, text="Export to Excel", command=self.export_to_excel).grid(row=9, column=0, columnspan=2, pady=10)

        # Display labels for the result
        ttk.Label(self.root, text="Result:").grid(row=10, column=0, columnspan=2, pady=5, sticky="w")
        ttk.Label(self.root, text="p-value:").grid(row=11, column=0, pady=5, sticky="w")
        ttk.Label(self.root, textvariable=self.p_value).grid(row=11, column=1, pady=5, sticky="w")

        ttk.Label(self.root, text="1 - p-value Level:").grid(row=12, column=0, pady=5, sticky="w")
        ttk.Label(self.root, textvariable=self.p_value_level).grid(row=12, column=1, pady=5, sticky="w")

        ttk.Label(self.root, text="Z-Score:").grid(row=13, column=0, pady=5, sticky="w")
        ttk.Label(self.root, textvariable=self.z_score).grid(row=13, column=1, pady=5, sticky="w")

        ttk.Label(self.root, text="Result:").grid(row=14, column=0, pady=5, sticky="w")
        ttk.Label(self.root, textvariable=self.significance).grid(row=14, column=1, pady=5, sticky="w")

    def perform_ab_test(self):
        try:
            # Get user input
            sample_size_a = self.sample_size_a.get()
            proportion_a = self.proportion_a.get()
            sample_size_b = self.sample_size_b.get()
            proportion_b = self.proportion_b.get()
            test_type = self.test_type.get()
            significance_threshold = float(self.significance_threshold.get())

            # Perform A/B test
            p_value, z_score, significance = self.calculate_ab_test(sample_size_a, proportion_a, sample_size_b, proportion_b, test_type)

            # Update result labels
            self.p_value.set(round(p_value, 2))

            # Calculate 1 - p-value level
            p_value_level = 1 - p_value
            self.p_value_level.set(round(p_value_level, 2))

            self.z_score.set(round(z_score, 2))

            # Determine statistical significance using 1 - p-value
            significance_text = f"Statistically Significant at {int(p_value_level * 100)}% level" if p_value < significance_threshold else "Not Statistically Significant"
            self.significance.set(significance_text)

        except ValueError:
            tk.messagebox.showerror("Error", "Please enter valid numeric values for sample sizes and proportions.")

    def export_to_excel(self):
        try:
            # Get user input
            sample_size_a = self.sample_size_a.get()
            proportion_a = self.proportion_a.get()
            sample_size_b = self.sample_size_b.get()
            proportion_b = self.proportion_b.get()
            test_type = self.test_type.get()
            significance_threshold = float(self.significance_threshold.get())

            # Get A/B test results
            p_value, z_score, significance = self.calculate_ab_test(sample_size_a, proportion_a, sample_size_b, proportion_b, test_type)

            # Create a DataFrame with the results
            data = {
                'Sample Size A': [sample_size_a],
                'Proportion A': [proportion_a],
                'Sample Size B': [sample_size_b],
                'Proportion B': [proportion_b],
                'p-value': [round(p_value, 3)],
                '1 - p-value Level': [round(1 - p_value, 3)],
                'Z-Score': [round(z_score, 3)],
                'Result': [significance],
                'Test Type': [test_type],
                'Significance Threshold': [significance_threshold]  # Added threshold info
            }

            df = pd.DataFrame(data)

            # Export DataFrame to Excel
            file_path = filedialog.asksaveasfilename(defaultextension=".xlsx", filetypes=[("Excel files", "*.xlsx")])
            if file_path:
                df.to_excel(file_path, index=False)
                messagebox.showinfo("Export Successful", f"Results exported to {file_path}")

        except ValueError:
            tk.messagebox.showerror("Error", "Please enter valid numeric values for sample sizes and proportions.")

    @staticmethod
    def calculate_ab_test(sample_size_a, proportion_a, sample_size_b, proportion_b, test_type):
        # Calculate success counts from proportions
        success_a = int(sample_size_a * proportion_a)
        success_b = int(sample_size_b * proportion_b)

        # Perform a two-sample z-test for proportions using statsmodels
        count = [success_a, success_b]
        nobs = [sample_size_a, sample_size_b]
        alternative = 'larger' if test_type == 'one-sided-greater' else 'smaller' if test_type == 'one-sided-less' else 'two-sided'
        z_stat, p_value = sm.stats.proportions_ztest(count, nobs, alternative=alternative)

        # Determine statistical significance
        significance = "Statistically Significant" if p_value < 0.05 else "Not Statistically Significant"

        return p_value, z_stat, significance

if __name__ == "__main__":
    root = tk.Tk()
    app = ABTestApp(root)
    root.mainloop()
