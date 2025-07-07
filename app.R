# Dashboard Climate Change Analysis - Jawa Timur
# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(leaflet)
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(VIM)
library(forecast)
library(tseries)
library(nortest)
library(robust)
library(MASS)
library(shinycssloaders)
library(rsconnect)
library(moments)


# Custom CSS theme for climate change
climate_css <- "
/* Import Google Fonts */
@import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700;800&family=JetBrains+Mono:wght@400;500&display=swap');

/* Root Variables for Consistent Theming */
:root {
  --primary-color: #064e3b;
  --primary-dark: #022c22;
  --primary-light: #065f46;
  --secondary-color: #059669;
  --secondary-dark: #047857;
  --secondary-light: #10b981;
  --accent-color: #fbbf24;
  --accent-light: #fcd34d;
  --danger-color: #dc2626;
  --warning-color: #f59e0b;
  --success-color: #059669;
  --info-color: #0891b2;
  
  --bg-primary: #ffffff;
  --bg-secondary: #f0fdf4;
  --bg-tertiary: #dcfce7;
  --bg-dark: #07290e;
  --bg-darker: #0a1b0b;
  
  --text-primary: #064e3b;
  --text-secondary: #166534;
  --text-light: #22c55e;
  --text-white: #ffffff;
  
  --border-color: #bbf7d0;
  --border-light: #dcfce7;
  
  --shadow-sm: 0 1px 2px 0 rgba(6, 78, 59, 0.05);
  --shadow-md: 0 4px 6px -1px rgba(6, 78, 59, 0.1), 0 2px 4px -1px rgba(6, 78, 59, 0.06);
  --shadow-lg: 0 10px 15px -3px rgba(6, 78, 59, 0.1), 0 4px 6px -2px rgba(6, 78, 59, 0.05);
  --shadow-xl: 0 20px 25px -5px rgba(6, 78, 59, 0.1), 0 10px 10px -5px rgba(6, 78, 59, 0.04);
  
  --gradient-primary: linear-gradient(135deg, #064e3b 0%, #022c22 50%, #065f46 100%);
  --gradient-secondary: linear-gradient(135deg, #059669 0%, #047857 50%, #10b981 100%);
  --gradient-success: linear-gradient(135deg, #22c55e 0%, #16a34a 50%, #15803d 100%);
  --gradient-climate: linear-gradient(135deg, #a7f3d0 0%, #d1fae5 50%, #bbf7d0 100%);
  --gradient-forest: linear-gradient(135deg, #14532d 0%, #166534 50%, #15803d 100%);
}

/* Global Styles */
* {
  box-sizing: border-box;
}

body {
  font-family: 'Poppins', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
  font-size: 17px;
  line-height: 1.6;
  color: var(--text-primary);
  background: var(--bg-secondary);
  margin: 0;
  padding: 0;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

/* Enhanced Typography */
h1, h2, h3, h4, h5, h6 {
  font-weight: 600;
  margin-bottom: 0.5em;
  color: var(--text-primary);
  font-family: 'Poppins', sans-serif;
}

h1 { font-size: 2.45rem; line-height: 1.2; }
h2 { font-size: 2.075rem; line-height: 1.3; }
h3 { font-size: 1.7rem; line-height: 1.4; }
h4 { font-size: 1.45rem; line-height: 1.4; }
h5 { font-size: 1.325rem; line-height: 1.5; }

p {
  margin-bottom: 1rem;
  color: var(--text-secondary);
}

/* Main Layout Styling */
.content-wrapper, .right-side {
  background: var(--bg-secondary) !important;
  min-height: 100vh;
}

/* Header Enhancements */
.main-header {
  background: var(--gradient-primary) !important;
  border: none !important;
  box-shadow: var(--shadow-lg);
  z-index: 1000;
}

.main-header .navbar {
  background: transparent !important;
  border: none !important;
}

.main-header .logo {
  background: transparent !important;
  color: var(--text-white) !important;
  font-weight: 700 !important;
  font-size: 1.6rem !important;
  transition: all 0.3s ease;
  display: flex;
  align-items: center;
  padding: 0 20px;
  font-family: 'Poppins', sans-serif !important;
  letter-spacing: 0.5px;
}

.main-header .logo:hover {
  transform: scale(1.02);
  background: rgba(255, 255, 255, 0.1) !important;
}

/* Sidebar Styling */
.main-sidebar {
  background: var(--bg-dark) !important;
  box-shadow: var(--shadow-xl);
  border-right: 1px solid rgba(255, 255, 255, 0.1);
}

.skin-blue .main-sidebar {
  background: var(--bg-dark) !important;
}

.sidebar-menu {
  margin: 0;
  padding: 20px 0;
}

.sidebar-menu > li {
  border-bottom: 1px solid rgba(255, 255, 255, 0.05);
  transition: all 0.3s ease;
}

.sidebar-menu > li > a {
  color: #cbd5e1 !important;
  padding: 18px 20px !important;
  font-weight: 600 !important;
  font-size: 1.3rem !important;
  transition: all 0.3s ease !important;
  border-left: 3px solid transparent;
  position: relative;
  overflow: hidden;
  font-family: 'Poppins', sans-serif !important;
  letter-spacing: 0.3px;
}

.sidebar-menu > li > a::before {
  content: '';
  position: absolute;
  top: 0;
  left: 0;
  width: 0;
  height: 100%;
  background: var(--gradient-primary);
  transition: width 0.3s ease;
  z-index: -1;
}

.sidebar-menu > li:hover > a::before {
  width: 100%;
}

.sidebar-menu > li:hover > a {
  color: var(--text-white) !important;
  border-left-color: var(--primary-light) !important;
  transform: translateX(5px);
  background: transparent !important;
}

.sidebar-menu > li.active > a {
  background: var(--gradient-primary) !important;
  color: var(--text-white) !important;
  border-left-color: var(--accent-color) !important;
  box-shadow: var(--shadow-md);
}

.sidebar-menu > li.active > a::before {
  width: 100%;
}

/* Sub-menu styling */
.sidebar-menu .treeview-menu {
  background: rgba(0, 0, 0, 0.2) !important;
  margin: 0;
}

.sidebar-menu .treeview-menu > li > a {
  color: #94a3b8 !important;
  padding: 14px 20px 14px 40px !important;
  font-size: 1.2rem !important;
  transition: all 0.3s ease !important;
  font-family: 'Poppins', sans-serif !important;
  font-weight: 500 !important;
  letter-spacing: 0.2px;
}

.sidebar-menu .treeview-menu > li:hover > a {
  color: var(--text-white) !important;
  background: rgba(255, 255, 255, 0.1) !important;
  transform: translateX(5px);
}

/* Enhanced Box Styling */
.box {
  background: var(--bg-primary);
  border: 1px solid var(--border-light);
  border-radius: 12px;
  box-shadow: var(--shadow-md);
  margin-bottom: 25px;
  overflow: visible !important;
  transition: all 0.3s ease;
  position: relative;
  height: auto;
  min-height: 200px;
  display: flex;
  flex-direction: column;
}

.box:hover {
  transform: translateY(-2px);
  box-shadow: var(--shadow-xl);
}

/* Ensure box content fills available space */
.box-body {
  flex: 1;
  display: flex;
  flex-direction: column;
  justify-content: space-between;
}

.box-header {
  background: var(--bg-primary) !important;
  border-bottom: 1px solid var(--border-light) !important;
  padding: 20px 25px !important;
  position: relative;
}

.box.box-solid.box-primary > .box-header {
  background: var(--gradient-primary) !important;
  color: var(--text-white) !important;
  border: none !important;
}

.box.box-solid.box-primary > .box-header .box-title {
  color: var(--text-white) !important;
  font-weight: 700 !important;
}

.box-title {
  font-size: 1.4rem !important;
  font-weight: 600 !important;
  margin: 0 !important;
  display: flex;
  align-items: center;
  gap: 8px;
  font-family: 'Poppins', sans-serif !important;
  letter-spacing: 0.3px;
}

.box-body {
  padding: 25px !important;
  background: var(--bg-primary);
}

/* Value Box Enhancements */
.small-box {
  border-radius: 12px !important;
  overflow: hidden !important;
  position: relative !important;
  transition: all 0.3s ease !important;
  box-shadow: var(--shadow-md) !important;
  border: none !important;
  background: var(--gradient-primary) !important;
}

.small-box:hover {
  transform: translateY(-5px) !important;
  box-shadow: var(--shadow-xl) !important;
}

.small-box .inner {
  padding: 20px !important;
  position: relative;
  z-index: 2;
}

.small-box .inner h3 {
  font-size: 2.6rem !important;
  font-weight: 700 !important;
  margin: 0 0 10px 0 !important;
  color: var(--text-white) !important;
  text-shadow: 0 2px 4px rgba(0,0,0,0.1);
  font-family: 'Poppins', sans-serif !important;
}

.small-box .inner p {
  font-size: 1.3rem !important;
  margin: 0 !important;
  color: rgba(255, 255, 255, 0.9) !important;
  font-weight: 600 !important;
  font-family: 'Poppins', sans-serif !important;
  letter-spacing: 0.3px;
}

.small-box .icon {
  position: absolute !important;
  top: 20px !important;
  right: 20px !important;
  z-index: 1 !important;
  font-size: 3rem !important;
  color: rgba(255, 255, 255, 0.2) !important;
  transition: all 0.3s ease !important;
}

.small-box:hover .icon {
  transform: scale(1.1) rotate(5deg);
  color: rgba(255, 255, 255, 0.3) !important;
}

/* Color variations for value boxes */
.bg-green .small-box {
  background: var(--gradient-success) !important;
}

.bg-blue .small-box {
  background: var(--gradient-primary) !important;
}

.bg-purple .small-box {
  background: var(--gradient-forest) !important;
}

.bg-orange .small-box {
  background: var(--gradient-secondary) !important;
}

/* Button Enhancements */
.btn {
  border-radius: 8px !important;
  padding: 14px 28px !important;
  font-weight: 600 !important;
  font-size: 1.2rem !important;
  border: none !important;
  transition: all 0.3s ease !important;
  text-transform: none !important;
  letter-spacing: 0.4px;
  position: relative;
  overflow: hidden;
  font-family: 'Poppins', sans-serif !important;
}

.btn::before {
  content: '';
  position: absolute;
  top: 0;
  left: -100%;
  width: 100%;
  height: 100%;
  background: linear-gradient(90deg, transparent, rgba(255,255,255,0.2), transparent);
  transition: left 0.5s;
}

.btn-danger {
  background: linear-gradient(135deg, var(--danger-color), #b91c1c, var(--danger-color)) !important;
  color: var(--text-white) !important;
  box-shadow: var(--shadow-md);
}

.btn-danger:hover {
  transform: translateY(-2px);
  background: linear-gradient(135deg, #991b1b, var(--danger-color)) !important;
  box-shadow: var(--shadow-lg);
}

.btn:hover::before {
  left: 100%;
}

.btn-primary {
  background: var(--gradient-primary) !important;
  color: var(--text-white) !important;
  box-shadow: var(--shadow-md);
}

.btn-primary:hover {
  transform: translateY(-2px) !important;
  box-shadow: var(--shadow-lg) !important;
  background: var(--gradient-primary) !important;
}

.btn-success {
  background: var(--gradient-success) !important;
  color: var(--text-white) !important;
  box-shadow: var(--shadow-md);
}

.btn-success:hover {
  transform: translateY(-2px) !important;
  box-shadow: var(--shadow-lg) !important;
}

.btn-block {
  width: 100% !important;
  margin-bottom: 15px !important;
}

/* Tab Navigation Enhancements */
.nav-tabs-custom {
  background: var(--bg-primary);
  border-radius: 12px;
  overflow: hidden;
  box-shadow: var(--shadow-md);
  margin-bottom: 25px;
}

.nav-tabs-custom > .nav-tabs {
  border-bottom: 1px solid var(--border-light) !important;
  background: var(--bg-secondary);
  margin: 0;
}

.nav-tabs-custom > .nav-tabs > li {
  margin-bottom: 0;
  border-radius: 0;
}

.nav-tabs-custom > .nav-tabs > li > a {
  border: none !important;
  border-radius: 0 !important;
  padding: 16px 22px !important;
  color: var(--text-primary) !important;
  font-weight: 600 !important;
  transition: all 0.3s ease !important;
  position: relative;
  font-family: 'Poppins', sans-serif !important;
  font-size: 1.2rem !important;
  letter-spacing: 0.3px;
}

.nav-tabs-custom > .nav-tabs > li > a::after {
  content: '';
  position: absolute;
  bottom: 0;
  left: 0;
  width: 0;
  height: 3px;
  background: var(--gradient-primary);
  transition: width 0.3s ease;
}

.nav-tabs-custom > .nav-tabs > li.active > a::after {
  width: 100%;
}

.nav-tabs-custom > .nav-tabs > li:hover > a {
  background: var(--primary-color) !important;
  color: var(--text-white) !important;
}

.nav-tabs-custom > .nav-tabs > li.active > a {
  background: var(--primary-color) !important;
  color: var(--text-white) !important;
  border-bottom: 3px solid var(--accent-color) !important;
  font-weight: 700 !important;
}

.nav-tabs-custom > .tab-content {
  padding: 25px;
  background: var(--bg-primary);
  color: var(--text-primary);
  overflow: visible !important;
}

/* Dark background text color override */
.nav-tabs-custom > .nav-tabs > li.active > a,
.nav-tabs-custom > .nav-tabs > li:hover > a {
  color: var(--text-white) !important;
}

/* Light background text color */
.nav-tabs-custom > .nav-tabs > li > a {
  color: var(--text-primary) !important;
}

/* Form Control Enhancements */
.form-control {
  border: 2px solid var(--border-color) !important;
  border-radius: 8px !important;
  padding: 14px 18px !important;
  font-size: 1.2rem !important;
  transition: all 0.3s ease !important;
  background: var(--bg-primary) !important;
  color: var(--text-primary) !important;
  font-family: 'Poppins', sans-serif !important;
  font-weight: 500 !important;
}

.form-control:focus {
  border-color: var(--primary-color) !important;
  box-shadow: 0 0 0 3px rgba(6, 78, 59, 0.15) !important;
  outline: none !important;
}

.form-group label {
  font-weight: 600 !important;
  color: var(--text-primary) !important;
  margin-bottom: 10px !important;
  font-size: 1.2rem !important;
  font-family: 'Poppins', sans-serif !important;
  letter-spacing: 0.2px;
}

/* Select2 and Selectize Enhancements */
.selectize-input {
  border: 2px solid var(--border-color) !important;
  border-radius: 8px !important;
  padding: 12px 16px !important;
  background: var(--bg-primary) !important;
  transition: all 0.3s ease !important;
}

.selectize-input.focus {
  border-color: var(--primary-color) !important;
  box-shadow: 0 0 0 3px rgba(6, 78, 59, 0.15) !important;
}

/* Enhanced Select Dropdown Styling */
.selectize-dropdown {
  background: var(--bg-primary) !important;
  border: 2px solid var(--primary-color) !important;
  border-radius: 8px !important;
  box-shadow: var(--shadow-lg) !important;
  max-height: 400px !important;
  overflow-y: auto !important;
  z-index: 9999 !important;
  position: absolute !important;
  width: 100% !important;
}

.selectize-dropdown-content {
  max-height: 380px !important;
  overflow-y: auto !important;
  overflow-x: hidden !important;
}

.selectize-dropdown .option {
  padding: 12px 16px !important;
  font-size: 1.1rem !important;
  color: var(--text-primary) !important;
  border-bottom: 1px solid var(--border-light) !important;
  transition: all 0.2s ease !important;
  font-family: 'Poppins', sans-serif !important;
  font-weight: 500 !important;
}

.selectize-dropdown .option:hover,
.selectize-dropdown .option.active {
  background: var(--primary-color) !important;
  color: var(--text-white) !important;
}

.selectize-dropdown .option:last-child {
  border-bottom: none !important;
}

/* Enhanced scrollbar for dropdown */
.selectize-dropdown::-webkit-scrollbar {
  width: 8px !important;
}

.selectize-dropdown::-webkit-scrollbar-track {
  background: var(--bg-tertiary) !important;
  border-radius: 4px !important;
}

.selectize-dropdown::-webkit-scrollbar-thumb {
  background: var(--primary-color) !important;
  border-radius: 4px !important;
  transition: background 0.3s ease !important;
}

.selectize-dropdown::-webkit-scrollbar-thumb:hover {
  background: var(--primary-dark) !important;
}

/* Fix for dropdown positioning and visibility */
.selectize-control.single .selectize-dropdown {
  border-top: none !important;
  margin-top: -1px !important;
}

.selectize-control.multi .selectize-dropdown {
  border-top: none !important;
  margin-top: -1px !important;
}

/* Ensure dropdown shows all content */
.selectize-dropdown {
  max-width: none !important;
  min-width: 100% !important;
  box-sizing: border-box !important;
}

/* Better scrolling behavior */
.selectize-dropdown-content {
  scroll-behavior: smooth !important;
}

/* Make sure dropdown is visible on mobile */
@media (max-width: 768px) {
  .selectize-dropdown {
    max-height: 300px !important;
    font-size: 16px !important; /* Prevent zoom on iOS */
  }
  
  .selectize-dropdown .option {
    padding: 14px 16px !important;
    font-size: 1.1rem !important;
  }
}

/* Override any container overflow that might clip dropdown */
.box, .box-body, .box-header, .fluidRow, .row, 
.col-sm-1, .col-sm-2, .col-sm-3, .col-sm-4, .col-sm-5, .col-sm-6, 
.col-sm-7, .col-sm-8, .col-sm-9, .col-sm-10, .col-sm-11, .col-sm-12,
.col-md-1, .col-md-2, .col-md-3, .col-md-4, .col-md-5, .col-md-6,
.col-md-7, .col-md-8, .col-md-9, .col-md-10, .col-md-11, .col-md-12,
.content-wrapper, .right-side, .main-sidebar, .tab-content, .tab-pane {
  overflow: visible !important;
}

.form-group {
  overflow: visible !important;
  position: relative !important;
  z-index: auto !important;
}

/* Ensure selectize container doesn't clip dropdown */
.selectize-control {
  overflow: visible !important;
  position: relative !important;
  z-index: 1000 !important;
}

.selectize-input {
  overflow: visible !important;
  z-index: 1001 !important;
}

/* Force dropdown to appear above everything with highest z-index */
.selectize-dropdown {
  transform: translateZ(0) !important;
  -webkit-transform: translateZ(0) !important;
  will-change: transform !important;
  position: fixed !important;
  z-index: 999999 !important;
  top: auto !important;
  left: auto !important;
  right: auto !important;
  bottom: auto !important;
}

/* Specific override for dropdown positioning */
.selectize-control.single .selectize-dropdown,
.selectize-control.multi .selectize-dropdown {
  position: absolute !important;
  z-index: 999999 !important;
  top: 100% !important;
  left: 0 !important;
  right: 0 !important;
  margin-top: -1px !important;
}

/* Alternative fix for clipping issues */
body > .selectize-dropdown,
.selectize-dropdown-content {
  z-index: 999999 !important;
  position: absolute !important;
  overflow: visible !important;
}

/* Override for dashboard specific containers */
.content, .content-header, .nav-tabs-custom, .tabsetPanel, .well {
  overflow: visible !important;
}

/* Shiny specific containers */
.shiny-input-container {
  overflow: visible !important;
  position: relative !important;
}

/* Data Table Enhancements */
.dataTables_wrapper {
  background: var(--bg-primary);
  border-radius: 12px;
  padding: 20px;
  box-shadow: var(--shadow-md);
  border: 1px solid var(--border-light);
}

table.dataTable {
  border-collapse: separate !important;
  border-spacing: 0 !important;
  border-radius: 8px !important;
  overflow: hidden !important;
}

table.dataTable thead th {
  background: var(--gradient-primary) !important;
  color: var(--text-white) !important;
  border: none !important;
  padding: 15px 12px !important;
  font-weight: 600 !important;
  text-align: center !important;
  position: relative !important;
}

table.dataTable tbody tr {
  transition: all 0.2s ease !important;
}

table.dataTable tbody tr:hover {
  background: rgba(6, 78, 59, 0.08) !important;
  transform: scale(1.01);
}

table.dataTable tbody td {
  padding: 12px !important;
  border-bottom: 1px solid var(--border-light) !important;
  vertical-align: middle !important;
}

/* Alert Enhancements */
.alert {
  border: none !important;
  border-radius: 12px !important;
  padding: 16px 20px !important;
  margin-bottom: 20px !important;
  font-weight: 500 !important;
  box-shadow: var(--shadow-md) !important;
  position: relative !important;
  overflow: hidden !important;
}

.alert::before {
  content: '';
  position: absolute;
  top: 0;
  left: 0;
  width: 4px;
  height: 100%;
  background: currentColor;
}

.alert-success {
  background: linear-gradient(135deg, #ecfdf5 0%, #d1fae5 100%) !important;
  color: var(--success-color) !important;
  border-left: 4px solid var(--success-color) !important;
}

.alert-info {
  background: linear-gradient(135deg, #eff6ff 0%, #dbeafe 100%) !important;
  color: var(--info-color) !important;
  border-left: 4px solid var(--info-color) !important;
}

.alert-warning {
  background: linear-gradient(135deg, #fffbeb 0%, #fef3c7 100%) !important;
  color: var(--warning-color) !important;
  border-left: 4px solid var(--warning-color) !important;
}

.alert-danger {
  background: linear-gradient(135deg, #fef2f2 0%, #fecaca 100%) !important;
  color: var(--danger-color) !important;
  border-left: 4px solid var(--danger-color) !important;
}

/* Code/Text Output Enhancements */
pre, .shiny-text-output {
  background: var(--bg-darker) !important;
  color: #e2e8f0 !important;
  border: 1px solid #374151 !important;
  border-radius: 8px !important;
  padding: 20px !important;
  font-family: 'JetBrains Mono', 'Monaco', 'Menlo', monospace !important;
  font-size: 1.05rem !important;
  line-height: 1.6 !important;
  overflow-x: auto !important;
  box-shadow: var(--shadow-md) !important;
  position: relative !important;
}

pre::before {
  content: '';
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 4px;
  background: var(--gradient-primary);
}

/* Loading Spinner Enhancements */
.spinner {
  margin: 40px auto !important;
  text-align: center !important;
}

.spinner > div {
  width: 12px !important;
  height: 12px !important;
  background: var(--primary-color) !important;
  border-radius: 100% !important;
  display: inline-block !important;
  animation: sk-bouncedelay 1.4s infinite ease-in-out both !important;
  margin: 0 2px !important;
}

@keyframes sk-bouncedelay {
  0%, 80%, 100% { 
    transform: scale(0);
  } 40% { 
    transform: scale(1.0);
  }
}

/* Plot Container Enhancements */
.plotly, .shiny-plot-output {
  border-radius: 12px !important;
  overflow: hidden !important;
  box-shadow: var(--shadow-md) !important;
  background: var(--bg-primary) !important;
  border: 1px solid var(--border-light) !important;
}

/* Custom scrollbar */
::-webkit-scrollbar {
  width: 8px;
  height: 8px;
}

::-webkit-scrollbar-track {
  background: var(--bg-tertiary);
  border-radius: 4px;
}

::-webkit-scrollbar-thumb {
  background: var(--primary-color);
  border-radius: 4px;
  transition: background 0.3s ease;
}

::-webkit-scrollbar-thumb:hover {
  background: var(--primary-dark);
}

/* Responsive Design */
@media (max-width: 768px) {
  .box-body {
    padding: 15px !important;
  }
  
  .main-header .logo {
    font-size: 1.4rem !important;
    padding: 0 15px !important;
  }
  
  .sidebar-menu > li > a {
    padding: 16px 15px !important;
    font-size: 1.2rem !important;
  }
  
  .small-box .inner {
    padding: 15px !important;
  }
  
  .small-box .inner h3 {
    font-size: 2rem !important;
  }
  
  .nav-tabs-custom > .nav-tabs > li > a {
    padding: 14px 16px !important;
    font-size: 1.1rem !important;
  }
}

/* Animation Classes */
@keyframes fadeInUp {
  from {
    opacity: 0;
    transform: translateY(30px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

@keyframes fadeInLeft {
  from {
    opacity: 0;
    transform: translateX(-30px);
  }
  to {
    opacity: 1;
    transform: translateX(0);
  }
}

@keyframes pulse {
  0% { transform: scale(1); }
  50% { transform: scale(1.05); }
  100% { transform: scale(1); }
}

.animate-fade-in-up {
  animation: fadeInUp 0.6s ease-out;
}

.animate-fade-in-left {
  animation: fadeInLeft 0.6s ease-out;
}

.animate-pulse {
  animation: pulse 2s infinite;
}

/* Custom utility classes */
.text-gradient {
  background: var(--gradient-primary);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
  font-weight: 700;
}

.bg-gradient-primary {
  background: var(--gradient-primary) !important;
}

.bg-gradient-success {
  background: var(--gradient-success) !important;
}

.bg-gradient-climate {
  background: var(--gradient-climate) !important;
}

/* Leaflet map enhancements */
.leaflet-container {
  border-radius: 12px !important;
  overflow: hidden !important;
  box-shadow: var(--shadow-lg) !important;
  border: 1px solid var(--border-light) !important;
}

.leaflet-popup-content-wrapper {
  border-radius: 8px !important;
  box-shadow: var(--shadow-xl) !important;
  border: none !important;
}

.leaflet-popup-content {
  font-family: 'Poppins', sans-serif !important;
  line-height: 1.5 !important;
  font-size: 1.15rem !important;
  font-weight: 500 !important;
}

/* File input enhancements */
.form-group input[type='file'] {
  border: 2px dashed var(--border-color) !important;
  border-radius: 8px !important;
  padding: 20px !important;
  background: var(--bg-secondary) !important;
  transition: all 0.3s ease !important;
  text-align: center !important;
}

.form-group input[type='file']:hover {
  border-color: var(--primary-color) !important;
  background: rgba(6, 78, 59, 0.08) !important;
}

/* Progress bar enhancements */
.progress {
  background: var(--bg-tertiary) !important;
  border-radius: 8px !important;
  overflow: hidden !important;
  box-shadow: inset 0 1px 3px rgba(0,0,0,0.1) !important;
}

.progress-bar {
  background: var(--gradient-primary) !important;
  transition: width 0.6s ease !important;
}

/* Enhanced focus states for accessibility */
*:focus {
  outline: 2px solid var(--primary-color) !important;
  outline-offset: 2px !important;
}

button:focus, 
.btn:focus {
  outline: 2px solid var(--primary-color) !important;
  outline-offset: 2px !important;
  box-shadow: 0 0 0 3px rgba(6, 78, 59, 0.15) !important;
}

/* Enhanced checkbox and radio styling */
input[type='checkbox'],
input[type='radio'] {
  width: 18px !important;
  height: 18px !important;
  accent-color: var(--primary-color) !important;
}

/* Improved list styling */
ul, ol {
  padding-left: 1.5rem;
}

li {
  margin-bottom: 0.5rem;
  color: var(--text-secondary);
}

/* Enhanced HR styling */
hr {
  border: none !important;
  height: 1px !important;
  background: linear-gradient(to right, transparent, var(--border-color), transparent) !important;
  margin: 1.5rem 0 !important;
}

/* Status indicators */
.status-indicator {
  display: inline-block;
  width: 8px;
  height: 8px;
  border-radius: 50%;
  margin-right: 8px;
}

.status-success { background: var(--success-color); }
.status-warning { background: var(--warning-color); }
.status-danger { background: var(--danger-color); }
.status-info { background: var(--info-color); }

/* Loading overlay */
.loading-overlay {
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background: rgba(0, 0, 0, 0.5);
  display: flex;
  justify-content: center;
  align-items: center;
  z-index: 9999;
}

.loading-spinner {
  width: 40px;
  height: 40px;
  border: 4px solid rgba(255, 255, 255, 0.3);
  border-top: 4px solid white;
  border-radius: 50%;
  animation: spin 1s linear infinite;
}

@keyframes spin {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
}
"

# Load and prepare data from Excel file
load_initial_data <- function(file_path = "1_DATA JATIM_GABUNG.xlsx") {
  
  # Baca data dari Excel
  raw_data <- read_excel(file_path, sheet = 1)
  
  # Bersihkan nama kolom sesuai dengan struktur asli
  colnames(raw_data) <- c(
    "Nama_Daerah",
    "Bulan", 
    "Tahun",
    "Produktivitas",
    "Curah_Hujan",
    "Suhu", 
    "Radiasi_Matahari",
    "Kelembapan",
    "NDVI",
    "CO",
    "Soil_Moisture"
  )
  
  # Konversi tipe data
  raw_data$Nama_Daerah <- as.character(raw_data$Nama_Daerah)
  raw_data$Bulan <- as.numeric(raw_data$Bulan)
  raw_data$Tahun <- as.numeric(raw_data$Tahun)
  raw_data$Produktivitas <- as.numeric(raw_data$Produktivitas)
  raw_data$Curah_Hujan <- as.numeric(raw_data$Curah_Hujan)
  raw_data$Suhu <- as.numeric(raw_data$Suhu)
  raw_data$Radiasi_Matahari <- as.numeric(raw_data$Radiasi_Matahari)
  raw_data$Kelembapan <- as.numeric(raw_data$Kelembapan)
  raw_data$NDVI <- as.numeric(raw_data$NDVI)
  raw_data$CO <- as.numeric(raw_data$CO)
  raw_data$Soil_Moisture <- as.numeric(raw_data$Soil_Moisture)
  
  # Hapus baris dengan NA values jika ada
  raw_data <- raw_data[complete.cases(raw_data), ]
  
  # Urutkan data berdasarkan daerah, tahun, dan bulan
  raw_data <- raw_data %>%
    arrange(Nama_Daerah, Tahun, Bulan) %>%
    mutate(
      # Buat ID unik untuk setiap observasi
      ID = paste(Nama_Daerah, Tahun, sprintf("%02d", Bulan), sep = "_"),
      
      # Tambahkan kategori daerah (Kota vs Kabupaten)
      Jenis_Daerah = ifelse(grepl("^Kota", Nama_Daerah), "Kota", "Kabupaten"),
      
      # Buat tanggal untuk analisis time series
      Tanggal = as.Date(paste(Tahun, Bulan, "01", sep = "-")),
      
      # Kategorisasi musim
      Musim = case_when(
        Bulan %in% c(12, 1, 2) ~ "Musim Hujan",
        Bulan %in% c(3, 4, 5) ~ "Peralihan I", 
        Bulan %in% c(6, 7, 8) ~ "Musim Kemarau",
        Bulan %in% c(9, 10, 11) ~ "Peralihan II"
      )
    )
  
  return(raw_data)
}

# UI
ui <- dashboardPage(
  title = "Climate Change Dashboard - Jawa Timur",
  
  # Header
  dashboardHeader(
    title = " Climate Change Dashboard - Jawa Timur",
    titleWidth = 450
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem(" Dashboard Overview", tabName = "overview", icon = icon("home")),
      menuItem(" Guide Penggunaan", tabName = "guide", icon = icon("question-circle")),
      menuItem(" Analisis Deskriptif", tabName = "descriptive", icon = icon("chart-bar")),
      menuItem(" Analisis Inferensia", tabName = "inferential", icon = icon("project-diagram"),
               menuSubItem("Korelasi Spearman", tabName = "correlation"),
               menuSubItem("Kruskal Wallis Test", tabName = "kruskal"),
               menuSubItem("Regresi Robust", tabName = "regression")
      ),
      menuItem(" Time Series Analysis", tabName = "timeseries", icon = icon("chart-line")),
      menuItem(" Prediksi & Simulasi", tabName = "prediction", icon = icon("magic")),
      menuItem(" Visualisasi Lanjutan", tabName = "advanced_viz", icon = icon("chart-pie")),
      menuItem("️ Visualisasi Spasial", tabName = "spatial", icon = icon("map")),
      menuItem(" Catatan Riset", tabName = "research", icon = icon("book")),
      menuItem("📋 Metadata", tabName = "metadata", icon = icon("info-circle")),
      menuItem("️ Download Data", tabName = "download", icon = icon("download")),
      menuItem("️ Upload Data", tabName = "upload", icon = icon("upload"))
    )
  ),
  
  # Body
  dashboardBody(
    
    tags$head(
      tags$style(HTML(climate_css)),
      tags$script(HTML("
        $(document).ready(function() {
          // Configure selectize dropdowns with better positioning
          $('select').selectize({
            maxOptions: 1000,  // Allow many options
            searchField: ['text', 'value'],
            placeholder: 'Pilih...',
            render: {
              option: function(data, escape) {
                return '<div class=\"option\">' + escape(data.text) + '</div>';
              }
            },
            onDropdownOpen: function() {
              // Ensure dropdown is positioned correctly and escapes container
              var dropdown = this.$dropdown;
              var control = this.$control;
              var controlOffset = control.offset();
              var controlHeight = control.outerHeight();
              var controlWidth = control.outerWidth();
              
              // Position dropdown relative to control but outside any containers
              dropdown.css({
                'position': 'absolute',
                'z-index': '999999',
                'max-height': '400px',
                'overflow-y': 'auto',
                'overflow-x': 'hidden',
                'width': controlWidth + 'px',
                'top': (controlOffset.top + controlHeight) + 'px',
                'left': controlOffset.left + 'px',
                'border': '2px solid #064e3b',
                'border-radius': '8px',
                'box-shadow': '0 10px 15px -3px rgba(6, 78, 59, 0.1)',
                'background': '#ffffff'
              });
              
              // Append to body to escape container constraints
              if (dropdown.parent()[0] !== document.body) {
                dropdown.appendTo('body');
              }
            },
            onDropdownClose: function() {
              // Clean up positioning
              var dropdown = this.$dropdown;
              dropdown.css({
                'position': '',
                'top': '',
                'left': '',
                'z-index': ''
              });
            }
          });
          
          // Re-initialize selectize when content updates
          $(document).on('shiny:inputchanged', function(event) {
            setTimeout(function() {
              $('select:not(.selectized)').each(function() {
                $(this).selectize({
                  maxOptions: 1000,
                  searchField: ['text', 'value'],
                  placeholder: 'Pilih...',
                  onDropdownOpen: function() {
                    var dropdown = this.$dropdown;
                    var control = this.$control;
                    var controlOffset = control.offset();
                    var controlHeight = control.outerHeight();
                    var controlWidth = control.outerWidth();
                    
                    dropdown.css({
                      'position': 'absolute',
                      'z-index': '999999',
                      'max-height': '400px',
                      'overflow-y': 'auto',
                      'width': controlWidth + 'px',
                      'top': (controlOffset.top + controlHeight) + 'px',
                      'left': controlOffset.left + 'px'
                    });
                    
                    if (dropdown.parent()[0] !== document.body) {
                      dropdown.appendTo('body');
                    }
                  }
                });
              });
            }, 100);
          });
          
          // Handle window resize to reposition dropdowns
          $(window).on('resize', function() {
            $('.selectize-dropdown').each(function() {
              var dropdown = $(this);
              if (dropdown.is(':visible')) {
                dropdown.hide();
              }
            });
          });
        });
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = " Climate Change Dashboard - Jawa Timur", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(8,
                           h4("Selamat datang di Dashboard Analisis Perubahan Iklim Jawa Timur"),
                           p("Dashboard ini menyediakan analisis komprehensif tentang data iklim dan produktivitas pertanian di 38 daerah Jawa Timur periode 2020-2024."),
                           br()
                    ),
                    column(4,
                           tags$div(
                             style = "text-align: center; margin-top: 20px;",
                             actionButton("show_youtube_popup", " Tonton Video Tutorial", 
                                          class = "btn btn-danger btn-lg",
                                          style = "margin: 10px;")
                           )
                    )
                  ),
                  fluidRow(
                    valueBoxOutput("total_regions", width = 3),
                    valueBoxOutput("total_years", width = 3),
                    valueBoxOutput("total_observations", width = 3),
                    valueBoxOutput("variables_count", width = 3)
                  )
                )
              ),
              fluidRow(
                box(
                  title = " Ringkasan Data Iklim", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("overview_climate_plot")
                ),
                box(
                  title = " Produktivitas vs Iklim", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("overview_productivity_plot")
                )
              )
      ),
      # Guide Tab
      tabItem(tabName = "guide",
              fluidRow(
                box(
                  title = " Panduan Penggunaan Dashboard Climate Change", status = "primary", solidHeader = TRUE, width = 12,
                  tabsetPanel(
                    tabPanel(" Penggunaan",
                             br(),
                             h4("Selamat Datang di Dashboard Climate Change Jawa Timur!"),
                             p("Dashboard ini dirancang untuk membantu Anda menganalisis data iklim dan produktivitas pertanian di Jawa Timur. Berikut adalah langkah-langkah untuk memulai:"),
                             
                             h5(" Dashboard Overview"),
                             p("• Mulai dari tab 'Dashboard Overview' untuk melihat ringkasan data"),
                             p("• Lihat informasi dasar seperti jumlah daerah, tahun observasi, dan variabel"),
                             p("• Perhatikan grafik ringkasan untuk mendapat gambaran umum"),
                             
                             h5(" Upload "),
                             p("• Jika Anda memiliki variabel tambahan, gunakan tab 'Upload Data'"),
                             p("• File harus berformat CSV/Excel dengan kolom: Nama_Daerah, Bulan, Tahun"),
                             p("• Setelah upload, data akan otomatis tersedia di semua fitur analisis"),
                             
                             h5(" Analisis Statistik "),
                             p("• Pilih jenis analisis sesuai kebutuhan Anda"),
                             p("• Setiap tab memiliki pengaturan filter dan parameter"),
                             p("• Hasil akan ditampilkan dalam bentuk grafik, tabel, dan interpretasi"),
                             
                             div(class = "alert alert-info",
                                 HTML("<strong> Tips:</strong> Mulai dengan Analisis Deskriptif untuk memahami karakteristik data, kemudian lanjut ke analisis yang lebih kompleks."))
                    ),
                    
                    tabPanel("Tips Analisis",
                             br(),
                             h4("Fitur-Fitur Analisis yang Tersedia:"),
                             
                             h5("Analisis Deskriptif"),
                             tags$ul(
                               tags$li("Statistik dasar (mean, median, std dev, dll.)"),
                               tags$li("Uji normalitas (Shapiro-Wilk, Anderson-Darling, KS)"),
                               tags$li("Visualisasi distribusi (histogram, boxplot)"),
                               tags$li("Filter berdasarkan tahun dan daerah")
                             ),
                             
                             h5("Analisis Korelasi Spearman"),
                             tags$ul(
                               tags$li("Mengukur hubungan monoton antar variabel"),
                               tags$li("Matriks korelasi visual dan numerik"),
                               tags$li("Filter per tahun dan per daerah"),
                               tags$li("Interpretasi kekuatan korelasi")
                             ),
                             
                             h5(" Kruskal Wallis Test"),
                             tags$ul(
                               tags$li("Uji perbedaan median antar kelompok"),
                               tags$li("Pengelompokan berdasarkan tahun atau daerah"),
                               tags$li("Visualisasi boxplot dengan interpretasi"),
                               tags$li("Cocok untuk data non-normal")
                             ),
                             
                             h5("Regresi Robust"),
                             tags$ul(
                               tags$li("Model regresi tahan outlier"),
                               tags$li("Pemilihan variabel dependen dan independen"),
                               tags$li("Plot residual dan prediksi vs aktual"),
                               tags$li("Interpretasi koefisien model")
                             ),
                             
                             h5("Time Series Analysis"),
                             tags$ul(
                               tags$li("Analisis tren temporal data iklim"),
                               tags$li("Uji stasioneritas (ADF, KPSS)"),
                               tags$li("Model ARIMA otomatis"),
                               tags$li("Interpretasi komponen trend dan seasonal")
                             ),
                             
                             h5("Prediksi & Simulasi"),
                             tags$ul(
                               tags$li("Prediksi berdasarkan input variabel iklim"),
                               tags$li("Confidence interval untuk prediksi"),
                               tags$li("Analisis sensitivitas"),
                               tags$li("Perbandingan dengan data historis")
                             )
                    ),
                    
                    tabPanel(" Visualisasi",
                             br(),
                             h4("Fitur Visualisasi yang Tersedia:"),
                             
                             h5("Visualisasi Lanjutan"),
                             tags$ul(
                               tags$li("Heatmap korelasi interaktif"),
                               tags$li("Scatter plot matrix"),
                               tags$li("Plot distribusi per tahun"),
                               tags$li("Radar chart profil iklim")
                             ),
                             
                             h5(" Visualisasi Spasial"),
                             tags$ul(
                               tags$li("Peta interaktif Jawa Timur"),
                               tags$li("Visualisasi data per daerah"),
                               tags$li("Filter berdasarkan tahun dan bulan"),
                               tags$li("Informasi statistik regional")
                             ),
                             
                             h5("Tips Visualisasi:"),
                             div(class = "alert alert-success",
                                 tags$ul(
                                   tags$li("Gunakan filter untuk fokus pada periode atau wilayah tertentu"),
                                   tags$li("Hover mouse pada grafik untuk informasi detail"),
                                   tags$li("Peta interaktif dapat di-zoom dan di-pan"),
                                   tags$li("Semua grafik dapat di-export sebagai gambar")
                                 ))
                    ),
                    
                    tabPanel("Data & Download",
                             br(),
                             h4("Mengelola Data dan Download:"),
                             
                             h5("Upload Data Tambahan"),
                             p("Format file yang didukung:"),
                             tags$ul(
                               tags$li("CSV (.csv)"),
                               tags$li("Excel (.xlsx, .xls)")
                             ),
                             p("Struktur file yang diperlukan:"),
                             tags$ul(
                               tags$li("Kolom wajib: Nama_Daerah, Bulan, Tahun"),
                               tags$li("Kolom tambahan: variabel baru yang ingin ditambahkan"),
                               tags$li("Nama daerah harus sesuai dengan data utama"),
                               tags$li("Periode (tahun-bulan) harus ada yang overlapping")
                             ),
                             
                             h5("Download Hasil"),
                             p("Anda dapat mendownload:"),
                             tags$ul(
                               tags$li("Data mentah (termasuk data yang sudah digabung)"),
                               tags$li("Hasil analisis dalam format CSV")
                             ),
                             
                             h5("Catatan Riset"),
                             p("Gunakan tab 'Catatan Riset' untuk:"),
                             tags$ul(
                               tags$li("Memahami metodologi yang digunakan"),
                               tags$li("Interpretasi hasil statistik"),
                               tags$li("Rujukan untuk penulisan laporan"),
                               tags$li("Implikasi kebijakan dari hasil analisis")
                             )
                    ),
                    
                    tabPanel("FAQ & Tips",
                             br(),
                             h4("Frequently Asked Questions:"),
                             
                             h5("Q: Data saya tidak muncul setelah di-upload?"),
                             p("A: Pastikan file memiliki kolom Nama_Daerah, Bulan, Tahun yang sesuai format. Periksa juga apakah nama daerah cocok dengan data utama."),
                             
                             h5("Q: Uji normalitas menunjukkan data tidak normal, apa yang harus dilakukan?"),
                             p("A: Ini normal untuk data iklim. Gunakan metode non-parametrik seperti Korelasi Spearman dan Kruskal-Wallis Test yang sudah tersedia."),
                             
                             h5("Q: Model regresi memberikan hasil yang aneh?"),
                             p("A: Periksa outlier dalam data. Regresi robust sudah menangani outlier, tapi data yang ekstrem tetap bisa mempengaruhi hasil."),
                             
                             h5("Q: Time series menunjukkan data non-stasioner?"),
                             p("A: Ini wajar untuk data iklim. Model ARIMA otomatis akan melakukan differencing yang diperlukan."),
                             
                             br(),
                             h4("Tips Penggunaan Optimal:"),
                             div(class = "alert alert-info",
                                 tags$ul(
                                   tags$li("Mulai dengan analisis deskriptif untuk memahami data"),
                                   tags$li("Gunakan filter tahun/daerah untuk analisis spesifik"),
                                   tags$li("Kombinasikan beberapa jenis analisis untuk insight yang lebih dalam"),
                                   tags$li("Perhatikan interpretasi statistik di setiap hasil"),
                                   tags$li("Simpan hasil penting dengan fitur download"),
                                   tags$li("Gunakan visualisasi spasial untuk memahami pola geografis")
                                 ))
                    )
                  )
                )
              )
      ),
      # Descriptive Analysis Tab
      tabItem(tabName = "descriptive",
              fluidRow(
                box(
                  title = "Pengaturan Analisis", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("desc_variable", "Pilih Variabel:", 
                              choices = c("Produktivitas", "Curah_Hujan", "Suhu", 
                                          "Radiasi_Matahari", "Kelembapan", 
                                          "NDVI", "CO", "Soil_Moisture")),
                  selectInput("desc_year", "Pilih Tahun:", choices = NULL),
                  selectInput("desc_region", "Pilih Daerah:", choices = NULL),
                  checkboxInput("normality_test", "Uji Normalitas", value = TRUE),
                  actionButton("run_descriptive", "Analisis", class = "btn-primary", width = "100%")
                ),
                box(
                  title = "Statistik Deskriptif", status = "primary", solidHeader = TRUE, width = 9,
                  verbatimTextOutput("descriptive_stats"),
                  br(),
                  conditionalPanel(
                    condition = "input.normality_test",
                    h4("Hasil Uji Normalitas"),
                    verbatimTextOutput("normality_results")
                  )
                )
              ),
              fluidRow(
                box(
                  title = " Distribusi Data", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("descriptive_histogram")
                ),
                box(
                  title = " Boxplot Data", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("descriptive_boxplot")
                )
              )
      ),
      
      # Correlation Analysis Tab
      tabItem(tabName = "correlation",
              fluidRow(
                box(
                  title = " Pengaturan Korelasi Spearman", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("corr_year", "Pilih Tahun:", choices = c("Semua", 2020:2024), selected = "Semua"),
                  selectInput("corr_region", "Pilih Daerah:", choices = NULL),
                  actionButton("run_correlation", " Analisis Korelasi", class = "btn-primary", width = "100%")
                ),
                box(
                  title = " Matriks Korelasi Spearman", status = "primary", solidHeader = TRUE, width = 9,
                  plotOutput("correlation_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Tabel Korelasi", status = "primary", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("correlation_table")
                )
              )
      ),
      
      # Kruskal Wallis Test Tab
      tabItem(tabName = "kruskal",
              fluidRow(
                box(
                  title = " Pengaturan Kruskal Wallis", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("kw_variable", "Pilih Variabel Dependen:", 
                              choices = c("Produktivitas", "Curah_Hujan", "Suhu", 
                                          "Radiasi_Matahari", "Kelembapan", 
                                          "NDVI", "CO", "Soil_Moisture")),
                  selectInput("kw_grouping", "Kelompokkan berdasarkan:", 
                              choices = c("Tahun", "Daerah"), selected = "Tahun"),
                  conditionalPanel(
                    condition = "input.kw_grouping == 'Tahun'",
                    selectInput("kw_region_filter", "Filter Daerah:", choices = NULL)
                  ),
                  conditionalPanel(
                    condition = "input.kw_grouping == 'Daerah'",
                    selectInput("kw_year_filter", "Filter Tahun:", choices = 2020:2024, selected = 2024)
                  ),
                  actionButton("run_kruskal", " Uji Kruskal Wallis", class = "btn-primary", width = "100%")
                ),
                box(
                  title = " Hasil Kruskal Wallis Test", status = "primary", solidHeader = TRUE, width = 9,
                  verbatimTextOutput("kruskal_results"),
                  br(),
                  plotlyOutput("kruskal_plot")
                )
              )
      ),
      
      # Robust Regression Tab
      tabItem(tabName = "regression",
              fluidRow(
                box(
                  title = " Pengaturan Regresi Robust", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("reg_dependent", "Variabel Dependen:", 
                              choices = c("Produktivitas", "Curah_Hujan", "Suhu", 
                                          "Radiasi_Matahari", "Kelembapan", 
                                          "NDVI", "CO", "Soil_Moisture"),
                              selected = "Produktivitas"),
                  checkboxGroupInput("reg_independent", "Variabel Independen:",
                                     choices = c("Curah_Hujan", "Suhu", 
                                                 "Radiasi_Matahari", "Kelembapan", 
                                                 "NDVI", "CO", "Soil_Moisture"),
                                     selected = c("Suhu", "Curah_Hujan", "NDVI")),
                  selectInput("reg_year", "Pilih Tahun:", choices = c("Semua", 2020:2024), selected = "Semua"),
                  selectInput("reg_region", "Pilih Daerah:", choices = NULL),
                  actionButton("run_regression", " Analisis Regresi", class = "btn-primary", width = "100%")
                ),
                box(
                  title = "Hasil Regresi Robust", status = "primary", solidHeader = TRUE, width = 9,
                  verbatimTextOutput("regression_results")
                )
              ),
              fluidRow(
                box(
                  title = " Plot Residual", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("regression_residual_plot")
                ),
                box(
                  title = " Prediksi vs Aktual", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("regression_prediction_plot")
                )
              )
      ),
      
      # Time Series Analysis Tab
      tabItem(tabName = "timeseries",
              fluidRow(
                box(
                  title = " Pengaturan Time Series", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("ts_variable", "Pilih Variabel:", 
                              choices = c("Produktivitas", "Curah_Hujan", "Suhu", 
                                          "Radiasi_Matahari", "Kelembapan", 
                                          "NDVI", "CO", "Soil_Moisture")),
                  selectInput("ts_region", "Pilih Daerah:", choices = NULL),
                  checkboxInput("ts_trend", "Tampilkan Trend", value = TRUE),
                  actionButton("run_timeseries", " Analisis Time Series", class = "btn-primary", width = "100%")
                ),
                box(
                  title = " Plot Time Series", status = "primary", solidHeader = TRUE, width = 9,
                  plotlyOutput("timeseries_plot")
                )
              ),
              fluidRow(
                box(
                  title = " Interpretasi Stasioneritas & Model", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(6,
                           h5(" Hasil Uji Stasioneritas:"),
                           verbatimTextOutput("timeseries_tests")
                    ),
                    column(6,
                           h5(" Model ARIMA Terbaik:"),
                           verbatimTextOutput("timeseries_model")
                    )
                  ),
                  br(),
                  h5(" Interpretasi:"),
                  verbatimTextOutput("timeseries_interpretation")
                )
              )
      ),
      
      # Prediction Tab
      tabItem(tabName = "prediction",
              fluidRow(
                box(
                  title = " Pengaturan Prediksi", status = "primary", solidHeader = TRUE, width = 4,
                  selectInput("pred_target", "Variabel Target:", 
                              choices = c("Produktivitas", "NDVI", "Suhu"), 
                              selected = "Produktivitas"),
                  h5("Input Nilai Variabel Prediktor:"),
                  numericInput("pred_rainfall", "Curah Hujan (mm/hari):", value = 10, min = 0, max = 20, step = 0.1),
                  numericInput("pred_suhu", "Suhu (°C):", value = 25, min = 15, max = 35, step = 0.1),
                  numericInput("pred_kelembapan", "Kelembapan (%):", value = 85, min = 60, max = 100, step = 1),
                  numericInput("pred_radiasi", "Radiasi Matahari (10⁴ j/m²):", value = 50, min = 30, max = 70, step = 1),
                  numericInput("pred_ndvi", "NDVI:", value = 0.6, min = 0.3, max = 0.9, step = 0.01),
                  numericInput("pred_co", "CO (mol/m²):", value = 0.025, min = 0.015, max = 0.04, step = 0.001),
                  numericInput("pred_soil", "Soil Moisture:", value = 0.4, min = 0.2, max = 0.6, step = 0.01),
                  actionButton("run_prediction", " Prediksi", class = "btn-success", width = "100%")
                ),
                box(
                  title = " Hasil Prediksi", status = "primary", solidHeader = TRUE, width = 8,
                  h4("Model yang Digunakan:"),
                  verbatimTextOutput("prediction_model_info"),
                  br(),
                  h4("Hasil Prediksi:"),
                  verbatimTextOutput("prediction_result"),
                  br(),
                  h4("Confidence Interval:"),
                  verbatimTextOutput("prediction_interval")
                )
              ),
              fluidRow(
                box(
                  title = " Prediksi vs Data Historis", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("prediction_comparison_plot")
                ),
                box(
                  title = " Sensitivity Analysis", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("sensitivity_plot")
                )
              )
      ),
      
      # Advanced Visualization Tab
      tabItem(tabName = "advanced_viz",
              fluidRow(
                box(
                  title = " Pengaturan Visualisasi", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("viz_type", "Jenis Visualisasi:", 
                              choices = c("Heatmap Korelasi" = "heatmap",
                                          "Scatter Matrix" = "scatter_matrix",
                                          "Distribution Plot" = "distribution",
                                          "Climate Radar" = "radar")),
                  conditionalPanel(
                    condition = "input.viz_type == 'distribution'",
                    selectInput("dist_variable", "Pilih Variabel:",
                                choices = c("Produktivitas", "Curah_Hujan", "Suhu", 
                                            "Radiasi_Matahari", "Kelembapan", 
                                            "NDVI", "CO", "Soil_Moisture"))
                  ),
                  conditionalPanel(
                    condition = "input.viz_type == 'radar'",
                    selectInput("radar_region", "Pilih Daerah:", choices = NULL)
                  ),
                  actionButton("update_viz", " Update Visualisasi", class = "btn-primary", width = "100%")
                ),
                box(
                  title = " Visualisasi Utama", status = "primary", solidHeader = TRUE, width = 9,
                  plotlyOutput("advanced_plot", height = "500px")
                )
              ),
              fluidRow(
                box(
                  title = " Plot Tambahan 1", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("additional_plot1")
                ),
                box(
                  title = " Plot Tambahan 2", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("additional_plot2")
                )
              )
      ),
      
      # Spatial Visualization Tab
      tabItem(tabName = "spatial",
              fluidRow(
                box(
                  title = " Pengaturan Peta", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("map_variable", "Pilih Variabel:", 
                              choices = c("Produktivitas", "Curah_Hujan", "Suhu", 
                                          "Radiasi_Matahari", "Kelembapan", 
                                          "NDVI", "CO", "Soil_Moisture")),
                  selectInput("map_year", "Pilih Tahun:", choices = 2020:2024, selected = 2024),
                  selectInput("map_month", "Pilih Bulan:", 
                              choices = setNames(1:12, month.name), selected = 12),
                  radioButtons("map_aggregation", "Agregasi:", 
                               choices = c("Rata-rata" = "mean", "Median" = "median", "Maksimum" = "max"),
                               selected = "mean"),
                  actionButton("update_map", "🗺️ Perbarui Peta", class = "btn-primary", width = "100%")
                ),
                box(
                  title = "🗺️ Peta Interaktif Jawa Timur", status = "primary", solidHeader = TRUE, width = 9,
                  leafletOutput("interactive_map", height = "600px")
                )
              )
      ),
      
      # Research Notes Tab
      tabItem(tabName = "research",
              fluidRow(
                box(
                  title = " Dokumentasi Penelitian", status = "primary", solidHeader = TRUE, width = 12,
                  tabsetPanel(
                    tabPanel(" Data yang Digunakan",
                             h4("Sumber Data"),
                             p("Data iklim dan produktivitas pertanian Jawa Timur periode 2020-2024"),
                             h4("Variabel yang Dianalisis:"),
                             tags$ul(
                               tags$li(" Produktivitas: Tingkat produktivitas pertanian"),
                               tags$li("️ Curah Hujan: mm per hari"),
                               tags$li("️ Suhu: Derajat Celsius"),
                               tags$li("️ Radiasi: Puluh ribu j/m² hari"),
                               tags$li(" Kelembapan: Persentase"),
                               tags$li(" NDVI: Normalized Difference Vegetation Index"),
                               tags$li("️ CO: Carbon Monoxide mol/m²"),
                               tags$li(" Soil Moisture: Kelembapan tanah")
                             ),
                             h4("Cakupan Geografis:"),
                             p("38 Kabupaten/Kota di Jawa Timur dengan total 2,280 observasi")
                    ),
                    
                    tabPanel(" Alur Analisis",
                             h4("Tahapan Analisis:"),
                             tags$ol(
                               tags$li(" Analisis Deskriptif: Statistik dasar dan uji normalitas"),
                               tags$li(" Analisis Korelasi: Spearman rank correlation"),
                               tags$li(" Uji Perbedaan: Kruskal Wallis test"),
                               tags$li(" Regresi Robust: Modeling hubungan antar variabel"),
                               tags$li(" Time Series: Analisis tren temporal"),
                               tags$li("️ Visualisasi Spasial: Pemetaan interaktif")
                             ),
                             h4("Metodologi:"),
                             p("Menggunakan pendekatan statistik non-parametrik karena sifat data iklim yang tidak selalu terdistribusi normal.")
                    ),
                    
                    tabPanel(" Hasil Penting",
                             h4("Temuan Utama:"),
                             tags$ul(
                               tags$li(" Variabilitas iklim antar daerah dan waktu"),
                               tags$li(" Korelasi antara variabel iklim dan produktivitas"),
                               tags$li(" Tren perubahan iklim jangka pendek"),
                               tags$li("️ Pola spasial distribusi variabel iklim")
                             ),
                             br(),
                             h4("Implikasi Kebijakan:"),
                             tags$ul(
                               tags$li(" Adaptasi pertanian terhadap perubahan iklim"),
                               tags$li(" Manajemen sumber daya air"),
                               tags$li(" Konservasi lahan dan vegetasi"),
                               tags$li(" Perencanaan pembangunan berkelanjutan")
                             )
                    ),
                    
                    tabPanel(" Interpretasi Statistik",
                             h4("Interpretasi Hasil Analisis:"),
                             br(),
                             h5(" Korelasi Spearman:"),
                             p("Mengukur kekuatan hubungan monoton antar variabel. Nilai mendekati ±1 menunjukkan hubungan yang kuat."),
                             br(),
                             h5(" Kruskal Wallis Test:"),
                             p("Menguji perbedaan median antar kelompok. P-value < 0.05 menunjukkan perbedaan signifikan."),
                             br(),
                             h5(" Regresi Robust:"),
                             p("Model yang tahan terhadap outlier, cocok untuk data iklim yang sering mengandung nilai ekstrem."),
                             br(),
                             h5(" Time Series:"),
                             p("Analisis komponen trend, seasonal, dan irregular untuk memahami pola temporal.")
                    )
                  )
                )
              )
      ),
      
      # Metadata Tab
      tabItem(tabName = "metadata",
              fluidRow(
                box(
                  title = "📋 Metadata Dashboard", status = "primary", solidHeader = TRUE, width = 12,
                  tabsetPanel(
                    tabPanel("📊 Struktural",
                             br(),
                             h4("Struktur Data dan Sistem"),
                             
                             h5("🏗️ Arsitektur Dashboard:"),
                             tags$ul(
                               tags$li("Platform: R Shiny Framework"),
                               tags$li("Frontend: HTML5, CSS3, Bootstrap"),
                               tags$li("Backend: R dengan packages statistik"),
                               tags$li("Visualisasi: plotly, ggplot2, leaflet"),
                               tags$li("Database: In-memory R data.frame")
                             ),
                             
                             br(),
                             h5("📁 Struktur Database:"),
                             tags$div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; font-family: monospace;",
                               "Dataset utama: 1_DATA JATIM_GABUNG.xlsx", br(),
                               "├── Dimensi: 2,280 baris × 11 kolom", br(),
                               "├── Kolom Identifikasi:", br(),
                               "│   ├── Nama_Daerah (38 kabupaten/kota)", br(),
                               "│   ├── Bulan (1-12)", br(),
                               "│   └── Tahun (2020-2024)", br(),
                               "└── Kolom Variabel Iklim:", br(),
                               "    ├── Produktivitas", br(),
                               "    ├── Curah_Hujan (mm/hari)", br(),
                               "    ├── Suhu (°C)", br(),
                               "    ├── Radiasi_Matahari (10⁴ j/m²)", br(),
                               "    ├── Kelembapan (%)", br(),
                               "    ├── NDVI (index)", br(),
                               "    ├── CO (mol/m²)", br(),
                               "    └── Soil_Moisture (ratio)"
                             ),
                             
                             br(),
                             h5("🔄 Alur Pemrosesan Data:"),
                             tags$ol(
                               tags$li("📥 Input: Excel/CSV → R data.frame"),
                               tags$li("🔍 Validasi: Cek kolom wajib dan tipe data"),
                               tags$li("🧹 Pembersihan: Handle missing values"),
                               tags$li("🔗 Integrasi: Merge dengan data upload"),
                               tags$li("📊 Analisis: Statistik dan modeling"),
                               tags$li("📈 Visualisasi: Interactive plots"),
                               tags$li("💾 Output: Download hasil analisis")
                             ),
                             
                             br(),
                             h5("⚙️ Konfigurasi Sistem:"),
                             tags$ul(
                               tags$li("Memory limit: 50MB per upload"),
                               tags$li("Session timeout: Auto (browser dependent)"),
                               tags$li("Concurrent users: Unlimited (server dependent)"),
                               tags$li("Export formats: PNG, PDF, CSV")
                             )
                    ),
                    
                    tabPanel("📚 Referensi",
                             br(),
                             h4("Referensi Metodologi dan Sumber"),
                             
                             h5("📖 Metode Statistik:"),
                             tags$ul(
                               tags$li(tags$b("Spearman Rank Correlation:"), 
                                      " Spearman, C. (1904). The proof and measurement of association between two things. American Journal of Psychology, 15, 72-101."),
                               tags$li(tags$b("Kruskal-Wallis Test:"), 
                                      " Kruskal, W.H. & Wallis, W.A. (1952). Use of ranks in one-criterion variance analysis. Journal of the American Statistical Association, 47(260), 583-621."),
                               tags$li(tags$b("Robust Regression:"), 
                                      " Huber, P.J. (1973). Robust regression: Asymptotics, conjectures and Monte Carlo. The Annals of Statistics, 1(5), 799-821."),
                               tags$li(tags$b("ARIMA Modeling:"), 
                                      " Box, G.E.P., Jenkins, G.M., Reinsel, G.C., & Ljung, G.M. (2015). Time Series Analysis: Forecasting and Control. 5th Edition, Wiley.")
                             ),
                             
                             br(),
                             h5("💻 Teknologi dan Packages:"),
                             
                             h6("Core R Packages:"),
                             tags$ul(
                               tags$li(tags$code("shiny"), " - Web application framework"),
                               tags$li(tags$code("shinydashboard"), " - Dashboard layout"),
                               tags$li(tags$code("DT"), " - Interactive data tables"),
                               tags$li(tags$code("plotly"), " - Interactive visualizations"),
                               tags$li(tags$code("leaflet"), " - Interactive maps")
                             ),
                             
                             h6("Data Processing:"),
                             tags$ul(
                               tags$li(tags$code("readxl"), " - Excel file reading"),
                               tags$li(tags$code("dplyr"), " - Data manipulation"),
                               tags$li(tags$code("tidyr"), " - Data tidying")
                             ),
                             
                             h6("Statistical Analysis:"),
                             tags$ul(
                               tags$li(tags$code("corrplot"), " - Correlation matrix visualization"),
                               tags$li(tags$code("VIM"), " - Missing value imputation"),
                               tags$li(tags$code("forecast"), " - Time series forecasting"),
                               tags$li(tags$code("tseries"), " - Time series analysis"),
                               tags$li(tags$code("nortest"), " - Normality tests"),
                               tags$li(tags$code("robust"), " - Robust statistical methods"),
                               tags$li(tags$code("MASS"), " - Modern applied statistics"),
                               tags$li(tags$code("moments"), " - Statistical moments")
                             ),
                             
                             br(),
                             h5("🌍 Sumber Data Iklim:"),
                             tags$ul(
                               tags$li("BMKG (Badan Meteorologi, Klimatologi, dan Geofisika)"),
                               tags$li("Copernicus Climate Data Store (CDS)"),
                               tags$li("NASA Earth Data"),
                               tags$li("BPS Jawa Timur (produktivitas pertanian)")
                             ),
                             
                             br(),
                             h5("📊 Standar Data Quality:"),
                             tags$ul(
                               tags$li("✅ Completeness: > 90% data tersedia per variabel"),
                               tags$li("✅ Consistency: Format standar untuk semua periode"),
                               tags$li("✅ Accuracy: Validasi dengan multiple sources"),
                               tags$li("✅ Timeliness: Updated data availability"),
                               tags$li("✅ Validity: Range checks untuk setiap variabel")
                             ),
                             
                             br(),
                             h5("🔗 External APIs dan Services:"),
                             tags$ul(
                               tags$li("OpenStreetMap: Base map untuk visualisasi spasial"),
                               tags$li("CartoDB: Alternative map tiles"),
                               tags$li("Google Fonts: Typography (Poppins, JetBrains Mono)")
                             )
                    )
                  )
                )
              )
      ),
      
      # Download Tab
      tabItem(tabName = "download",
              fluidRow(
                box(
                  title = "⬇Download Data Hasil", status = "primary", solidHeader = TRUE, width = 12,
                  h4("Pilih data yang ingin didownload:"),
                  br(),
                  fluidRow(
                    column(6,
                           h5("Data Mentah"),
                           downloadButton("download_raw_data", "Download Data Lengkap", class = "btn-primary btn-block"),
                           br(), br(),
                           p("Data lengkap (asli + upload) dalam format CSV dengan semua variabel")
                    ),
                    column(6,
                           h5(" Hasil Analisis"),
                           downloadButton("download_analysis", "Download Hasil Analisis", class = "btn-success btn-block"),
                           br(), br(),
                           p("Hasil korelasi, regresi, dan uji statistik dalam format CSV")
                    )
                  ),
                  br(),
                  fluidRow(
                    column(12,
                           h5(" Informasi Dataset:"),
                           verbatimTextOutput("dataset_info")
                    )
                  )
                )
              )
      ),
      
      # Upload Tab - MODIFIED (AUTO MERGE)
      tabItem(tabName = "upload",
              fluidRow(
                box(
                  title = " Upload Data Tambahan", status = "primary", solidHeader = TRUE, width = 12,
                  h4("Upload data untuk menambah variabel atau periode baru"),
                  p("File harus berformat CSV atau Excel dengan kolom wajib: Nama_Daerah, Bulan, Tahun"),
                  p(tags$b("Catatan: Data akan otomatis digabungkan dengan dataset utama setelah validasi")),
                  p(tags$b("Maksimum ukuran file: 50MB")),
                  br(),
                  fileInput("upload_file", "Pilih File:",
                            accept = c(".csv", ".xlsx", ".xls"),
                            buttonLabel = "Browse...",
                            placeholder = "Belum ada file yang dipilih"),
                  br(),
                  conditionalPanel(
                    condition = "output.file_uploaded",
                    fluidRow(
                      column(6,
                             h5("Status Upload:"),
                             verbatimTextOutput("upload_status")
                      ),
                      column(6,
                             h5(" Preview Data Upload:"),
                             DT::dataTableOutput("uploaded_preview")
                      )
                    ),
                    br(),
                    conditionalPanel(
                      condition = "output.auto_merged",
                      h5(" Data Berhasil Digabungkan!"),
                      verbatimTextOutput("merge_summary"),
                      br(),
                      h5(" Preview Data Setelah Penggabungan:"),
                      DT::dataTableOutput("final_preview")
                    )
                  )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Load initial data
  data <- reactive({
    load_initial_data()
  })
  
  # Update region choices
  observe({
    current_data <- if(values$data_merged) values$merged_data else values$current_data
    if(!is.null(current_data)) {
      # Sort regions alphabetically for better user experience
      regions <- sort(unique(current_data$Nama_Daerah))
      years <- sort(unique(current_data$Tahun))
      
      # Update choices for descriptive analysis with proper formatting
      updateSelectInput(session, "desc_year", 
                        choices = c("Semua" = "All", setNames(years, years)), 
                        selected = "All")
      updateSelectInput(session, "desc_region", 
                        choices = c("Semua" = "All", setNames(regions, regions)), 
                        selected = "All")
      
      # Update other choices with sorted regions
      updateSelectInput(session, "corr_region", 
                        choices = c("Semua", regions), 
                        selected = "Semua")
      updateSelectInput(session, "kw_region_filter", 
                        choices = c("Semua", regions), 
                        selected = "Semua")
      updateSelectInput(session, "reg_region", 
                        choices = c("Semua", regions), 
                        selected = "Semua")
      updateSelectInput(session, "ts_region", 
                        choices = regions, 
                        selected = if(length(regions) > 0) regions[1] else NULL)
      updateSelectInput(session, "radar_region", 
                        choices = regions, 
                        selected = if(length(regions) > 0) regions[1] else NULL)
    }
  })
  observeEvent(input$show_youtube_popup, {
    showModal(modalDialog(
      title = " Video Tutorial Dashboard",
      size = "l",
      easyClose = TRUE,
      tags$div(
        style = "text-align: center;",
        tags$iframe(
          src = "https://www.youtube.com/embed/kPfD-I4R9QM",
          width = "100%",
          height = "400px",
          frameborder = "0",
          allowfullscreen = TRUE
        )
      ),
      footer = tagList(
        modalButton("Tutup"),
        actionButton("open_youtube", "Buka di YouTube",  
                     onclick = "window.open('https://www.youtube.com/watch?v=kPfD-I4R9QM', '_blank')",
                     class = "btn-primary")
      )
    ))
  })
  
  # Tambahkan output untuk value boxes dan plots
  output$total_regions <- renderValueBox({
    valueBox(
      value = "38",
      subtitle = "Total Daerah",
      icon = icon("map-marker-alt"),
      color = "green"
    )
  })
  
  output$total_years <- renderValueBox({
    valueBox(
      value = "5",
      subtitle = "Periode Tahun",
      icon = icon("calendar"),
      color = "blue"
    )
  })
  
  output$total_observations <- renderValueBox({
    valueBox(
      value = "2,280",
      subtitle = "Total Observasi",
      icon = icon("database"),
      color = "yellow"
    )
  })
  
  output$variables_count <- renderValueBox({
    valueBox(
      value = "11",
      subtitle = "Variabel Iklim",
      icon = icon("thermometer-half"),
      color = "red"
    )
  })
  
  # Placeholder plots
  output$overview_climate_plot <- renderPlotly({
    plot_ly(x = ~c(1,2,3), y = ~c(1,2,3), type = 'scatter', mode = 'lines') %>%
      layout(title = "Climate Overview")
  })
  
  output$overview_productivity_plot <- renderPlotly({
    plot_ly(x = ~c(1,2,3), y = ~c(1,2,3), type = 'scatter', mode = 'markers') %>%
      layout(title = "Productivity Overview")
  })
  
  # Prediction & Simulation (use current_data)
  observeEvent(input$run_prediction, {
    df <- current_data()
    
    # Prepare input data for prediction
    new_data <- data.frame(
      Curah_Hujan = input$pred_rainfall,
      Suhu = input$pred_suhu,
      Kelembapan = input$pred_kelembapan,
      Radiasi_Matahari = input$pred_radiasi,
      NDVI = input$pred_ndvi,
      CO = input$pred_co,
      Soil_Moisture = input$pred_soil
    )
    
    # Build prediction model based on target variable
    if(input$pred_target == "Produktivitas") {
      predictors <- c("Curah_Hujan", "Suhu", "Kelembapan", 
                      "Radiasi_Matahari", "NDVI", "CO", "Soil_Moisture")
    } else if(input$pred_target == "NDVI") {
      predictors <- c("Curah_Hujan", "Suhu", "Kelembapan", 
                      "Radiasi_Matahari", "CO", "Soil_Moisture")
    } else {
      predictors <- c("Curah_Hujan", "Kelembapan", 
                      "Radiasi_Matahari", "NDVI", "CO", "Soil_Moisture")
    }
    
    # Remove target from predictors if it exists
    predictors <- predictors[predictors != input$pred_target]
    
    tryCatch({
      # Build robust regression model
      formula_str <- paste(input$pred_target, "~", paste(predictors, collapse = " + "))
      pred_model <- rlm(as.formula(formula_str), data = df, method = "M")
      
      # Make prediction
      prediction <- predict(pred_model, newdata = new_data[predictors])
      
      # Calculate prediction interval (more robust approach)
      model_residuals <- residuals(pred_model)
      residual_se <- mad(model_residuals)  # Median Absolute Deviation for robust SE
      conf_interval <- c(prediction - 1.96 * residual_se, prediction + 1.96 * residual_se)
      
      # Calculate model performance metrics
      fitted_values <- fitted(pred_model)
      actual_values <- df[[input$pred_target]]
      r_squared <- cor(fitted_values, actual_values)^2
      mae <- mean(abs(fitted_values - actual_values), na.rm = TRUE)
      
      output$prediction_model_info <- renderText({
        paste(
          "Model: Robust Regression (M-estimator)\n",
          "Formula:", formula_str, "\n",
          "Training samples:", nrow(df), "\n",
          "Model Performance:\n",
          "- R-squared:", round(r_squared, 4), "\n",
          "- MAE:", round(mae, 4), "\n",
          "- Robust SE:", round(residual_se, 4)
        )
      })
      
      # Determine prediction quality
      prediction_quality <- if(r_squared > 0.7) {
        "BAIK (R² > 0.7)"
      } else if(r_squared > 0.5) {
        "SEDANG (R² 0.5-0.7)"
      } else {
        "KURANG (R² < 0.5)"
      }
      
      output$prediction_result <- renderText({
        paste(
          "HASIL PREDIKSI ", input$pred_target, ":\n",
          "================================\n",
          "Nilai prediksi: ", round(prediction, 4), "\n",
          "Kualitas model: ", prediction_quality, "\n\n",
          "INPUT VARIABEL:\n",
          "- Curah Hujan: ", input$pred_rainfall, " mm/hari\n",
          "- Suhu: ", input$pred_suhu, " °C\n",
          "- Kelembapan: ", input$pred_kelembapan, " %\n",
          "- Radiasi Matahari: ", input$pred_radiasi, " x10⁴ j/m²\n",
          "- NDVI: ", input$pred_ndvi, "\n",
          "- CO: ", input$pred_co, " mol/m²\n",
          "- Soil Moisture: ", input$pred_soil
        )
      })
      
      output$prediction_interval <- renderText({
        interval_width <- diff(conf_interval)
        interval_percentage <- (interval_width / abs(prediction)) * 100
        
        paste(
          "95% CONFIDENCE INTERVAL:\n",
          "========================\n",
          "Lower bound: ", round(conf_interval[1], 4), "\n",
          "Upper bound: ", round(conf_interval[2], 4), "\n",
          "Interval width: ", round(interval_width, 4), "\n",
          "Uncertainty: ±", round(interval_percentage, 1), "%\n\n",
          "INTERPRETASI:\n",
          if(interval_percentage < 10) {
            "Prediksi sangat akurat (uncertainty < 10%)"
          } else if(interval_percentage < 20) {
            "Prediksi cukup akurat (uncertainty 10-20%)"
          } else {
            "Prediksi kurang akurat (uncertainty > 20%)"
          }
        )
      })
      
      # Comparison plot with historical data (FIXED)
      output$prediction_comparison_plot <- renderPlotly({
        tryCatch({
          # Get historical data statistics
          hist_mean <- mean(df[[input$pred_target]], na.rm = TRUE)
          hist_sd <- sd(df[[input$pred_target]], na.rm = TRUE)
          hist_min <- min(df[[input$pred_target]], na.rm = TRUE)
          hist_max <- max(df[[input$pred_target]], na.rm = TRUE)
          
          # Create comparison data
          comparison_data <- data.frame(
            Kategori = c("Historis Min", "Historis Mean", "Historis Max", "Prediksi"),
            Nilai = c(hist_min, hist_mean, hist_max, prediction),
            Type = c("Historical", "Historical", "Historical", "Prediction"),
            Color = c("#4ECDC4", "#2E8B57", "#4ECDC4", "#FF6B6B")
          )
          
          p <- ggplot(comparison_data, aes(x = Kategori, y = Nilai, fill = Color)) +
            geom_col(alpha = 0.8, width = 0.6) +
            geom_text(aes(label = round(Nilai, 3)), vjust = -0.5, size = 4, fontface = "bold") +
            labs(title = paste("Perbandingan Prediksi vs Data Historis -", input$pred_target),
                 subtitle = paste("Prediksi berada di",
                                  ifelse(prediction < hist_mean, "bawah", "atas"), "rata-rata historis"),
                 x = "Kategori", y = input$pred_target) +
            theme_minimal() +
            theme(legend.position = "none",
                  plot.title = element_text(size = 14, face = "bold"),
                  axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_fill_identity()
          
          ggplotly(p, tooltip = c("x", "y"))
        }, error = function(e) {
          plot_ly() %>% 
            add_annotations(text = paste("Error in comparison plot:", e$message),
                            x = 0.5, y = 0.5, xref = "paper", yref = "paper")
        })
      })
      
      # Sensitivity analysis (IMPROVED)
      output$sensitivity_plot <- renderPlotly({
        tryCatch({
          # Choose the most important predictor based on model coefficients
          model_coefs <- abs(coef(pred_model)[-1])  # Exclude intercept
          vary_var <- names(model_coefs)[which.max(model_coefs)]
          
          if(vary_var %in% predictors) {
            sensitivity_data <- data.frame()
            base_value <- new_data[[vary_var]]
            var_range <- seq(from = base_value * 0.7, 
                             to = base_value * 1.3, 
                             length.out = 20)
            
            for(val in var_range) {
              temp_data <- new_data[predictors]
              temp_data[[vary_var]] <- val
              pred_val <- predict(pred_model, newdata = temp_data)
              sensitivity_data <- rbind(sensitivity_data, 
                                        data.frame(Variable_Value = val, 
                                                   Prediction = pred_val,
                                                   Change_Pct = ((val - base_value) / base_value) * 100))
            }
            
            p <- ggplot(sensitivity_data, aes(x = Change_Pct, y = Prediction)) +
              geom_line(color = "#2E8B57", size = 1.5) +
              geom_point(color = "#FF6B6B", size = 2) +
              geom_vline(xintercept = 0, linetype = "dashed", color = "blue", alpha = 0.7) +
              geom_hline(yintercept = prediction, linetype = "dashed", color = "red", alpha = 0.7) +
              labs(title = paste("Analisis Sensitivitas:", vary_var, "vs", input$pred_target),
                   subtitle = paste("Perubahan", vary_var, "terhadap prediksi", input$pred_target),
                   x = paste("Perubahan", vary_var, "(%)"), 
                   y = paste("Prediksi", input$pred_target)) +
              theme_minimal() +
              theme(plot.title = element_text(size = 12, face = "bold"))
            
            ggplotly(p, tooltip = c("x", "y"))
          } else {
            plot_ly() %>% 
              add_annotations(text = "Sensitivity analysis tidak tersedia untuk kombinasi ini",
                              x = 0.5, y = 0.5, xref = "paper", yref = "paper")
          }
        }, error = function(e) {
          plot_ly() %>% 
            add_annotations(text = paste("Error in sensitivity analysis:", e$message),
                            x = 0.5, y = 0.5, xref = "paper", yref = "paper")
        })
      })
      
    }, error = function(e) {
      output$prediction_result <- renderText({
        paste("Error dalam prediksi:", e$message, 
              "\nPastikan semua variabel prediktor tersedia dan model dapat dibangun.")
      })
    })
  })
  
  # Advanced Visualizations (use current_data)
  observeEvent(input$update_viz, {
    df <- current_data()
    
    if(input$viz_type == "heatmap") {
      output$advanced_plot <- renderPlotly({
        numeric_cols <- c("Produktivitas", "Curah_Hujan", "Suhu", 
                          "Radiasi_Matahari", "Kelembapan", 
                          "NDVI", "CO", "Soil_Moisture")
        corr_matrix <- cor(df[numeric_cols], use = "complete.obs")
        
        plot_ly(z = corr_matrix, type = "heatmap", 
                x = colnames(corr_matrix), y = rownames(corr_matrix),
                colorscale = list(c(0, "#FF6B6B"), c(0.5, "white"), c(1, "#4ECDC4")),
                hovertemplate = "%{x} vs %{y}<br>Korelasi: %{z:.3f}<extra></extra>") %>%
          layout(title = "Heatmap Korelasi Antar Variabel Iklim",
                 annotations = list(
                   list(x = 0.5, y = -0.15, xref = "paper", yref = "paper",
                        text = "Interpretasi: Biru = korelasi positif kuat, Merah = korelasi negatif kuat, Putih = tidak berkorelasi",
                        showarrow = FALSE, font = list(size = 12))
                 ))
      })
      
    } else if(input$viz_type == "scatter_matrix") {
      output$advanced_plot <- renderPlotly({
        # Select key variables for scatter matrix
        key_vars <- c("Produktivitas", "Curah_Hujan", "Suhu", "NDVI")
        scatter_data <- df[key_vars]
        
        # Create scatter plot matrix using plotly
        fig <- plot_ly(type = "splom",
                       dimensions = list(
                         list(label = "Produktivitas", values = ~Produktivitas),
                         list(label = "Curah Hujan", values = ~Curah_Hujan),
                         list(label = "Suhu", values = ~Suhu),
                         list(label = "NDVI", values = ~NDVI)
                       ),
                       data = scatter_data,
                       marker = list(color = "#2E8B57", size = 4, opacity = 0.6))
        
        fig %>% layout(
          title = "Scatter Plot Matrix - Hubungan Antar Variabel Utama",
          annotations = list(
            list(x = 0.5, y = -0.1, xref = "paper", yref = "paper",
                 text = "Interpretasi: Diagonal menunjukkan distribusi, off-diagonal menunjukkan hubungan antar variabel",
                 showarrow = FALSE, font = list(size = 12))
          )
        )
      })
      
    } else if(input$viz_type == "distribution") {
      output$advanced_plot <- renderPlotly({
        p <- ggplot(df, aes_string(x = input$dist_variable)) +
          geom_histogram(aes(y = ..density..), bins = 30, fill = "#4ECDC4", alpha = 0.7, color = "white") +
          geom_density(color = "#2E8B57", size = 1.5) +
          facet_wrap(~Tahun, scales = "free_y") +
          labs(title = paste("Distribusi", input$dist_variable, "per Tahun"),
               subtitle = "Garis menunjukkan kurva kepadatan, histogram menunjukkan frekuensi",
               x = input$dist_variable, y = "Density") +
          theme_minimal() +
          theme(plot.title = element_text(size = 14, face = "bold"))
        
        ggplotly(p) %>%
          layout(annotations = list(
            list(x = 0.5, y = -0.15, xref = "paper", yref = "paper",
                 text = "Interpretasi: Perhatikan perubahan bentuk distribusi antar tahun - indikasi perubahan iklim",
                 showarrow = FALSE, font = list(size = 12))
          ))
      })
      
    } else if(input$viz_type == "trend_comparison") {
      output$advanced_plot <- renderPlotly({
        # Normalize data for comparison
        trend_data <- df %>%
          group_by(Tahun, Bulan) %>%
          summarise_at(input$trend_variables, mean, na.rm = TRUE, .groups = 'drop') %>%
          mutate(Date = as.Date(paste(Tahun, Bulan, "01", sep = "-"))) %>%
          pivot_longer(cols = input$trend_variables, names_to = "Variable", values_to = "Value") %>%
          group_by(Variable) %>%
          mutate(Normalized_Value = scale(Value)[,1])  # Z-score normalization
        
        p <- ggplot(trend_data, aes(x = Date, y = Normalized_Value, color = Variable)) +
          geom_line(size = 1.2, alpha = 0.8) +
          geom_smooth(method = "loess", se = FALSE, linetype = "dashed", size = 0.8) +
          labs(title = "Perbandingan Tren Variabel (Normalized)",
               subtitle = "Data dinormalisasi (Z-score) untuk perbandingan yang fair",
               x = "Tanggal", y = "Nilai Normalized (Z-score)") +
          theme_minimal() +
          theme(plot.title = element_text(size = 14, face = "bold")) +
          scale_color_brewer(palette = "Set1") +
          geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5)
        
        ggplotly(p) %>%
          layout(annotations = list(
            list(x = 0.5, y = -0.2, xref = "paper", yref = "paper",
                 text = "Interpretasi: Garis putus-putus = tren jangka panjang, Nilai > 0 = di atas rata-rata, < 0 = di bawah rata-rata",
                 showarrow = FALSE, font = list(size = 11))
          ))
      })
      
    } else if(input$viz_type == "radar") {
      output$advanced_plot <- renderPlotly({
        # Create radar chart data for selected region
        radar_data <- df %>%
          filter(Nama_Daerah == input$radar_region) %>%
          summarise(
            Produktivitas = mean(Produktivitas, na.rm = TRUE),
            Curah_Hujan = mean(Curah_Hujan, na.rm = TRUE),
            Suhu = mean(Suhu, na.rm = TRUE),
            Kelembapan = mean(Kelembapan, na.rm = TRUE),
            NDVI = mean(NDVI, na.rm = TRUE),
            .groups = 'drop'
          )
        
        # Normalize to 0-100 scale for radar chart
        radar_normalized <- data.frame(
          Variable = c("Produktivitas", "Curah_Hujan", "Suhu", "Kelembapan", "NDVI"),
          Value = c(
            (radar_data$Produktivitas / max(df$Produktivitas, na.rm = TRUE)) * 100,
            (radar_data$Curah_Hujan / max(df$Curah_Hujan, na.rm = TRUE)) * 100,
            (radar_data$Suhu / max(df$Suhu, na.rm = TRUE)) * 100,
            (radar_data$Kelembapan / max(df$Kelembapan, na.rm = TRUE)) * 100,
            (radar_data$NDVI / max(df$NDVI, na.rm = TRUE)) * 100
          )
        )
        
        # Create radar chart
        fig <- plot_ly(
          type = 'scatterpolar',
          r = c(radar_normalized$Value, radar_normalized$Value[1]),  # Close the shape
          theta = c(radar_normalized$Variable, radar_normalized$Variable[1]),
          fill = 'toself',
          fillcolor = 'rgba(46, 139, 87, 0.4)',
          line = list(color = '#2E8B57', width = 3),
          marker = list(color = '#FF6B6B', size = 8)
        ) %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = TRUE,
                range = c(0, 100),
                tickvals = c(0, 25, 50, 75, 100),
                ticktext = c("0%", "25%", "50%", "75%", "100%")
              )
            ),
            title = paste("Climate Profile Radar Chart -", input$radar_region),
            annotations = list(
              list(x = 0.5, y = -0.1, xref = "paper", yref = "paper",
                   text = "Interpretasi: Semakin luar = nilai relatif semakin tinggi terhadap maksimum regional",
                   showarrow = FALSE, font = list(size = 12))
            )
          )
        
        fig
      })
      
    } else if(input$viz_type == "seasonal") {
      output$advanced_plot <- renderPlotly({
        seasonal_data <- df %>%
          mutate(Season = case_when(
            Bulan %in% c(12, 1, 2) ~ "Musim Hujan",
            Bulan %in% c(3, 4, 5) ~ "Peralihan I", 
            Bulan %in% c(6, 7, 8) ~ "Musim Kemarau",
            Bulan %in% c(9, 10, 11) ~ "Peralihan II"
          )) %>%
          group_by(Season, Tahun) %>%
          summarise(
            Produktivitas = mean(Produktivitas, na.rm = TRUE),
            Suhu = mean(Suhu, na.rm = TRUE),
            Curah_Hujan = mean(Curah_Hujan, na.rm = TRUE),
            NDVI = mean(NDVI, na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          pivot_longer(cols = c("Produktivitas", "Suhu", "Curah_Hujan", "NDVI"), 
                       names_to = "Variable", values_to = "Value")
        
        p <- ggplot(seasonal_data, aes(x = Season, y = Value, fill = Variable)) +
          geom_boxplot(alpha = 0.7, position = "dodge") +
          facet_wrap(~Variable, scales = "free_y", ncol = 2) +
          labs(title = "Pola Musiman Variabel Iklim dan Produktivitas",
               subtitle = "Boxplot menunjukkan variasi antar tahun untuk setiap musim",
               x = "Musim", y = "Nilai") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(size = 14, face = "bold"),
                legend.position = "none") +
          scale_fill_brewer(palette = "Set2")
        
        ggplotly(p) %>%
          layout(annotations = list(
            list(x = 0.5, y = -0.15, xref = "paper", yref = "paper",
                 text = "Interpretasi: Perhatikan pola konsisten vs variasi tinggi antar musim - indikator stabilitas iklim",
                 showarrow = FALSE, font = list(size = 11))
          ))
      })
    }
    
    # Additional plots with interpretations
    output$additional_plot1 <- renderPlotly({
      # Enhanced boxplot by year with trend
      yearly_summary <- df %>%
        group_by(Tahun) %>%
        summarise(
          Mean_Prod = mean(Produktivitas, na.rm = TRUE),
          SD_Prod = sd(Produktivitas, na.rm = TRUE),
          .groups = 'drop'
        )
      
      p1 <- ggplot(df, aes(x = as.factor(Tahun), y = Produktivitas)) +
        geom_boxplot(aes(fill = as.factor(Tahun)), alpha = 0.7, outlier.alpha = 0.5) +
        geom_point(data = yearly_summary, aes(x = as.factor(Tahun), y = Mean_Prod), 
                   color = "red", size = 3, shape = 18) +
        geom_line(data = yearly_summary, aes(x = as.numeric(as.factor(Tahun)), y = Mean_Prod), 
                  color = "red", size = 1, alpha = 0.8, group = 1) +
        labs(title = "Produktivitas per Tahun dengan Tren", 
             subtitle = "Diamond merah = rata-rata tahunan, Garis merah = tren",
             x = "Tahun", y = "Produktivitas") +
        theme_minimal() +
        theme(legend.position = "none", plot.title = element_text(size = 12, face = "bold")) +
        scale_fill_brewer(palette = "Spectral")
      
      ggplotly(p1, tooltip = c("x", "y"))
    })
    
    output$additional_plot2 <- renderPlotly({
      # Enhanced scatter plot with regression and confidence interval
      model_simple <- lm(Produktivitas ~ Suhu, data = df)
      r_squared <- summary(model_simple)$r.squared
      
      p2 <- ggplot(df, aes(x = Suhu, y = Produktivitas)) +
        geom_point(aes(color = as.factor(Tahun)), alpha = 0.6, size = 2) +
        geom_smooth(method = "lm", se = TRUE, color = "#2E8B57", fill = "#4ECDC4", alpha = 0.3) +
        labs(title = paste("Hubungan Suhu vs Produktivitas (R² =", round(r_squared, 3), ")"),
             subtitle = ifelse(r_squared > 0.5, "Korelasi KUAT", 
                               ifelse(r_squared > 0.3, "Korelasi SEDANG", "Korelasi LEMAH")),
             x = "Suhu (°C)", y = "Produktivitas", color = "Tahun") +
        theme_minimal() +
        theme(plot.title = element_text(size = 12, face = "bold")) +
        scale_color_brewer(palette = "Set1")
      
      ggplotly(p2, tooltip = c("x", "y", "colour"))
    })
  })
  
  # File upload handling with enhanced merge functionality
  values <- reactiveValues(
    current_data = NULL,
    correlation_result = NULL,
    regression_result = NULL,
    uploaded_data = NULL,
    merged_data = NULL,
    data_merged = FALSE
  )
  
  observe({
    values$current_data <- data()
  })
  
  output$file_uploaded <- reactive({
    return(!is.null(input$upload_file))
  })
  outputOptions(output, 'file_uploaded', suspendWhenHidden = FALSE)
  
  output$data_merged <- reactive({
    return(values$data_merged)
  })
  outputOptions(output, 'data_merged', suspendWhenHidden = FALSE)
  
  observeEvent(input$upload_file, {
    req(input$upload_file)
    
    # Check file size (50MB limit)
    file_size_mb <- input$upload_file$size / (1024 * 1024)
    if(file_size_mb > 50) {
      output$merge_status <- renderText({
        paste("ERROR: File terlalu besar (", round(file_size_mb, 1), 
              "MB). Maksimum 50MB diizinkan.")
      })
      return()
    }
    
    tryCatch({
      ext <- tools::file_ext(input$upload_file$datapath)
      
      if (ext == "csv") {
        uploaded_data <- read.csv(input$upload_file$datapath, stringsAsFactors = FALSE)
      } else if (ext %in% c("xlsx", "xls")) {
        uploaded_data <- read_excel(input$upload_file$datapath)
      } else {
        output$merge_status <- renderText("ERROR: Format file tidak didukung. Gunakan CSV atau Excel.")
        return()
      }
      
      # Validate required columns
      required_cols <- c("Nama_Daerah", "Bulan", "Tahun")
      missing_cols <- setdiff(required_cols, colnames(uploaded_data))
      
      if(length(missing_cols) > 0) {
        output$merge_status <- renderText({
          paste("ERROR: Kolom wajib tidak ditemukan:", paste(missing_cols, collapse = ", "),
                "\nFile harus memiliki kolom: Nama_Daerah, Bulan, Tahun")
        })
        return()
      }
      
      # Store uploaded data
      values$uploaded_data <- uploaded_data
      
      # Check data compatibility
      main_regions <- unique(values$current_data$Nama_Daerah)
      upload_regions <- unique(uploaded_data$Nama_Daerah)
      common_regions <- intersect(main_regions, upload_regions)
      
      main_periods <- unique(paste(values$current_data$Tahun, values$current_data$Bulan))
      upload_periods <- unique(paste(uploaded_data$Tahun, uploaded_data$Bulan))
      common_periods <- intersect(main_periods, upload_periods)
      
      output$merge_status <- renderText({
        paste(
          "VALIDASI DATA UPLOAD:\n",
          "====================\n",
          "✓ File berhasil dibaca (", round(file_size_mb, 1), "MB)\n",
          "✓ Format file: ", toupper(ext), "\n",
          "✓ Jumlah baris: ", nrow(uploaded_data), "\n",
          "✓ Jumlah kolom: ", ncol(uploaded_data), "\n",
          "✓ Kolom tambahan: ", paste(setdiff(colnames(uploaded_data), required_cols), collapse = ", "), "\n\n",
          "KOMPATIBILITAS:\n",
          "- Daerah cocok: ", length(common_regions), "/", length(upload_regions), 
          " (", round(length(common_regions)/length(upload_regions)*100, 1), "%)\n",
          "- Periode cocok: ", length(common_periods), "/", length(upload_periods),
          " (", round(length(common_periods)/length(upload_periods)*100, 1), "%)\n\n",
          "STATUS: ", 
          ifelse(length(common_regions) > 0 && length(common_periods) > 0,
                 "✓ SIAP DIGABUNG", "✗ TIDAK KOMPATIBEL")
        )
      })
      
      output$uploaded_preview <- DT::renderDataTable({
        DT::datatable(uploaded_data, 
                      options = list(scrollX = TRUE, pageLength = 5),
                      caption = paste("Preview Data Upload -", nrow(uploaded_data), "baris"))
      })
      
    }, error = function(e) {
      output$merge_status <- renderText({
        paste("ERROR membaca file:", e$message)
      })
    })
  })
  
  observeEvent(input$merge_data, {
    req(values$uploaded_data)
    
    tryCatch({
      # Perform merge
      merged_data <- merge(values$current_data, values$uploaded_data,
                           by = c("Nama_Daerah", "Bulan", "Tahun"),
                           all.x = TRUE, suffixes = c("", "_new"))
      
      values$merged_data <- merged_data
      values$data_merged <- TRUE
      
      # Calculate merge success statistics
      total_main <- nrow(values$current_data)
      total_merged <- nrow(merged_data)
      new_columns <- setdiff(colnames(merged_data), colnames(values$current_data))
      
      output$merge_status <- renderText({
        paste(
          "PENGGABUNGAN BERHASIL!\n",
          "======================\n",
          "✓ Data utama: ", total_main, " baris\n",
          "✓ Data gabungan: ", total_merged, " baris\n",
          "✓ Kolom baru ditambahkan: ", length(new_columns), "\n",
          "✓ Nama kolom baru: ", paste(new_columns, collapse = ", "), "\n\n",
          "Data siap digunakan untuk analisis!"
        )
      })
      
      output$merged_preview <- DT::renderDataTable({
        DT::datatable(values$merged_data, 
                      options = list(scrollX = TRUE, pageLength = 5),
                      caption = paste("Data Setelah Digabung -", nrow(values$merged_data), "baris,", ncol(values$merged_data), "kolom"))
      })
      
    }, error = function(e) {
      output$merge_status <- renderText({
        paste("ERROR dalam penggabungan:", e$message)
      })
    })
  })
  
  # Get current data (original or merged)
  current_data <- reactive({
    if(values$data_merged && !is.null(values$merged_data)) {
      return(values$merged_data)
    } else {
      return(values$current_data)
    }
  })
  
  # Overview value boxes (use current_data)
  output$total_regions <- renderValueBox({
    valueBox(
      value = length(unique(current_data()$Nama_Daerah)),
      subtitle = "Total Daerah",
      icon = icon("map-marker-alt"),
      color = "green"
    )
  })
  
  output$total_years <- renderValueBox({
    valueBox(
      value = length(unique(current_data()$Tahun)),
      subtitle = "Periode Tahun",
      icon = icon("calendar"),
      color = "blue"
    )
  })
  
  output$total_observations <- renderValueBox({
    valueBox(
      value = nrow(current_data()),
      subtitle = "Total Observasi",
      icon = icon("database"),
      color = "purple"
    )
  })
  
  output$variables_count <- renderValueBox({
    valueBox(
      value = ncol(current_data()) - 3,
      subtitle = "Variabel Iklim",
      icon = icon("thermometer-half"),
      color = "orange"
    )
  })
  
  # Overview plots (use current_data)
  output$overview_climate_plot <- renderPlotly({
    df <- current_data() %>%
      group_by(Tahun) %>%
      summarise(
        Suhu = mean(Suhu, na.rm = TRUE),
        Curah_Hujan = mean(Curah_Hujan, na.rm = TRUE),
        Kelembapan = mean(Kelembapan, na.rm = TRUE),
        .groups = 'drop'
      )
    
    p <- ggplot(df, aes(x = Tahun)) +
      geom_line(aes(y = Suhu, color = "Suhu (°C)"), size = 1.2) +
      geom_line(aes(y = Curah_Hujan, color = "Curah Hujan (mm/hari)"), size = 1.2) +
      geom_line(aes(y = Kelembapan, color = "Kelembapan (%)"), size = 1.2) +
      labs(title = "Tren Variabel Iklim Utama", x = "Tahun", y = "Nilai") +
      theme_minimal() +
      scale_color_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1")) +
      theme(legend.title = element_blank())
    
    ggplotly(p)
  })
  
  output$overview_productivity_plot <- renderPlotly({
    df <- current_data() %>%
      group_by(Tahun) %>%
      summarise(
        Produktivitas = mean(Produktivitas, na.rm = TRUE),
        NDVI = mean(NDVI, na.rm = TRUE),
        .groups = 'drop'
      )
    
    p <- ggplot(df, aes(x = Tahun)) +
      geom_bar(aes(y = Produktivitas), stat = "identity", fill = "#96CEB4", alpha = 0.7) +
      geom_line(aes(y = NDVI * 10, color = "NDVI x10"), size = 1.5) +
      labs(title = "Produktivitas dan NDVI", x = "Tahun", y = "Produktivitas") +
      scale_y_continuous(sec.axis = sec_axis(~./10, name = "NDVI")) +
      theme_minimal() +
      scale_color_manual(values = "#FF6B6B") +
      theme(legend.title = element_blank())
    
    ggplotly(p)
  })
  
  # Descriptive Analysis (use current_data)
  observeEvent(input$run_descriptive, {
    df <- current_data()
    
    # Filter by year
    if (input$desc_year != "All") {
      df <- df[df$Tahun == as.numeric(input$desc_year), ]
    }
    
    # Filter by region
    if (input$desc_region != "All") {
      df <- df[df$Nama_Daerah == input$desc_region, ]
    }
    
    variable_data <- df[[input$desc_variable]]
    
    output$descriptive_stats <- renderText({
      stats <- paste(
        "Statistik Deskriptif untuk", input$desc_variable, "\n",
        "=====================================\n",
        "Filter: Tahun =", ifelse(input$desc_year == "All", "Semua", input$desc_year),
        ", Daerah =", ifelse(input$desc_region == "All", "Semua", input$desc_region), "\n",
        "Jumlah Observasi:", length(variable_data), "\n",
        "Mean:", round(mean(variable_data, na.rm = TRUE), 4), "\n",
        "Median:", round(median(variable_data, na.rm = TRUE), 4), "\n",
        "Std Dev:", round(sd(variable_data, na.rm = TRUE), 4), "\n",
        "Min:", round(min(variable_data, na.rm = TRUE), 4), "\n",
        "Max:", round(max(variable_data, na.rm = TRUE), 4), "\n",
        "Q1:", round(quantile(variable_data, 0.25, na.rm = TRUE), 4), "\n",
        "Q3:", round(quantile(variable_data, 0.75, na.rm = TRUE), 4), "\n",
        "Skewness:", round(moments::skewness(variable_data, na.rm = TRUE), 4), "\n",
        "Kurtosis:", round(moments::kurtosis(variable_data, na.rm = TRUE), 4)
      )
      stats
    })
    
    if (input$normality_test) {
      output$normality_results <- renderText({
        # Shapiro-Wilk test (if sample size <= 5000)
        if (length(variable_data) <= 5000 && length(variable_data) >= 3) {
          shapiro_test <- shapiro.test(variable_data)
          shapiro_result <- paste("Shapiro-Wilk Test: p-value =", round(shapiro_test$p.value, 6))
          shapiro_interp <- ifelse(shapiro_test$p.value < 0.05, "Data TIDAK normal", "Data normal")
        } else {
          shapiro_result <- "Shapiro-Wilk Test: Data terlalu besar/kecil"
          shapiro_interp <- ""
        }
        
        # Anderson-Darling test
        tryCatch({
          ad_test <- ad.test(variable_data)
          ad_result <- paste("Anderson-Darling Test: p-value =", round(ad_test$p.value, 6))
          ad_interp <- ifelse(ad_test$p.value < 0.05, "Data TIDAK normal", "Data normal")
        }, error = function(e) {
          ad_result <- "Anderson-Darling Test: Error"
          ad_interp <- ""
        })
        
        # Kolmogorov-Smirnov test
        tryCatch({
          ks_test <- ks.test(variable_data, "pnorm", mean(variable_data, na.rm = TRUE), sd(variable_data, na.rm = TRUE))
          ks_result <- paste("Kolmogorov-Smirnov Test: p-value =", round(ks_test$p.value, 6))
          ks_interp <- ifelse(ks_test$p.value < 0.05, "Data TIDAK normal", "Data normal")
        }, error = function(e) {
          ks_result <- "KS Test: Error"
          ks_interp <- ""
        })
        
        paste(
          "Uji Normalitas\n",
          "================\n",
          shapiro_result, ifelse(shapiro_interp != "", paste(" -", shapiro_interp), ""), "\n",
          ad_result, ifelse(exists("ad_interp") && ad_interp != "", paste(" -", ad_interp), ""), "\n",
          ks_result, ifelse(exists("ks_interp") && ks_interp != "", paste(" -", ks_interp), ""), "\n\n",
          "Interpretasi: p-value < 0.05 menunjukkan data tidak terdistribusi normal"
        )
      })
    }
    
    output$descriptive_histogram <- renderPlotly({
      p <- ggplot(df, aes_string(x = input$desc_variable)) +
        geom_histogram(bins = 30, fill = "#2E8B57", alpha = 0.7, color = "white") +
        labs(title = paste("Distribusi", input$desc_variable), x = input$desc_variable, y = "Frekuensi") +
        theme_minimal() +
        theme(plot.title = element_text(color = "#1E6B4F", size = 14, face = "bold"))
      
      ggplotly(p)
    })
    
    output$descriptive_boxplot <- renderPlotly({
      p <- ggplot(df, aes_string(x = "1", y = input$desc_variable)) +
        geom_boxplot(fill = "#4ECDC4", alpha = 0.7, color = "#1E6B4F") +
        labs(title = paste("Boxplot", input$desc_variable), 
             y = input$desc_variable, x = "") +
        theme_minimal() +
        theme(plot.title = element_text(color = "#1E6B4F", size = 14, face = "bold"),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
      
      ggplotly(p)
    })
  })
  
  # Correlation Analysis (use current_data)
  observeEvent(input$run_correlation, {
    df <- current_data()
    
    if (input$corr_year != "Semua") {
      df <- df[df$Tahun == input$corr_year, ]
    }
    if (input$corr_region != "Semua") {
      df <- df[df$Nama_Daerah == input$corr_region, ]
    }
    
    numeric_cols <- c("Produktivitas", "Curah_Hujan", "Suhu", 
                      "Radiasi_Matahari", "Kelembapan", 
                      "NDVI", "CO", "Soil_Moisture")
    
    corr_data <- df[numeric_cols]
    corr_matrix <- cor(corr_data, method = "spearman", use = "complete.obs")
    
    values$correlation_result <- corr_matrix
    
    output$correlation_plot <- renderPlot({
      corrplot(corr_matrix, method = "color", type = "upper", order = "hclust",
               tl.col = "black", tl.srt = 45, tl.cex = 0.9,
               col = colorRampPalette(c("#FF6B6B", "white", "#4ECDC4"))(200),
               addCoef.col = "black", number.cex = 0.7)
      
      # Add interpretation
      mtext("Interpretasi: Warna biru = korelasi positif, Warna merah = korelasi negatif\nAngka mendekati ±1 = korelasi kuat, mendekati 0 = korelasi lemah", 
            side = 1, line = 4, cex = 0.8)
    })
    
    output$correlation_table <- DT::renderDataTable({
      corr_df <- as.data.frame(round(corr_matrix, 3))
      corr_df$Variable <- rownames(corr_df)
      corr_df <- corr_df[, c(ncol(corr_df), 1:(ncol(corr_df)-1))]
      
      DT::datatable(corr_df, 
                    options = list(
                      scrollX = TRUE, 
                      pageLength = -1,
                      searching = FALSE,
                      paging = FALSE,
                      info = FALSE,
                      ordering = TRUE,
                      dom = 't'
                    ),
                    caption = paste("Korelasi Spearman - Filter: Tahun =", 
                                    ifelse(input$corr_year == "Semua", "Semua", input$corr_year),
                                    ", Daerah =", 
                                    ifelse(input$corr_region == "Semua", "Semua", input$corr_region),
                                    "\nInterpretasi: Nilai 0.7-1.0 = korelasi sangat kuat, 0.3-0.7 = kuat, 0.1-0.3 = lemah, <0.1 = sangat lemah"))
    })
  })
  
  # Kruskal Wallis Test (use current_data)
  observeEvent(input$run_kruskal, {
    df <- current_data()
    
    # Ensure we have enough data
    if(nrow(df) < 10) {
      output$kruskal_results <- renderText("Error: Tidak cukup data untuk analisis")
      return()
    }
    
    if (input$kw_grouping == "Tahun") {
      if (input$kw_region_filter != "Semua") {
        df <- df[df$Nama_Daerah == input$kw_region_filter, ]
      }
      grouping_var <- "Tahun"
      df$Tahun <- as.factor(df$Tahun)  # Convert to factor for proper grouping
    } else {
      df <- df[df$Tahun == input$kw_year_filter, ]
      grouping_var <- "Nama_Daerah"
      # Limit to top 10 regions if too many for visualization
      top_regions <- df %>% 
        group_by(Nama_Daerah) %>% 
        summarise(mean_val = mean(.data[[input$kw_variable]], na.rm = TRUE), .groups = 'drop') %>%
        top_n(10, mean_val) %>%
        pull(Nama_Daerah)
      df <- df[df$Nama_Daerah %in% top_regions, ]
    }
    
    # Check if we still have enough groups and data
    n_groups <- length(unique(df[[grouping_var]]))
    if(n_groups < 2) {
      output$kruskal_results <- renderText("Error: Perlu minimal 2 kelompok untuk analisis")
      return()
    }
    
    # Perform Kruskal-Wallis test
    tryCatch({
      formula_str <- paste(input$kw_variable, "~", grouping_var)
      kw_result <- kruskal.test(as.formula(formula_str), data = df)
      
      # Calculate group medians for interpretation
      group_medians <- df %>%
        group_by(.data[[grouping_var]]) %>%
        summarise(
          n = n(),
          median = median(.data[[input$kw_variable]], na.rm = TRUE),
          mean = mean(.data[[input$kw_variable]], na.rm = TRUE),
          .groups = 'drop'
        )
      
      output$kruskal_results <- renderText({
        interpretation <- if(kw_result$p.value < 0.001) {
          "Ada perbedaan SANGAT signifikan antar kelompok (p < 0.001)"
        } else if(kw_result$p.value < 0.01) {
          "Ada perbedaan SANGAT signifikan antar kelompok (p < 0.01)"
        } else if(kw_result$p.value < 0.05) {
          "Ada perbedaan signifikan antar kelompok (p < 0.05)"
        } else {
          "TIDAK ada perbedaan signifikan antar kelompok (p ≥ 0.05)"
        }
        
        group_summary <- paste(group_medians[[grouping_var]], ": median =", 
                               round(group_medians$median, 3), collapse = "\n")
        
        paste(
          "Uji Kruskal-Wallis\n",
          "==================\n",
          "Variabel:", input$kw_variable, "\n",
          "Kelompok:", grouping_var, "\n",
          "Jumlah kelompok:", n_groups, "\n",
          "Total observasi:", nrow(df), "\n\n",
          "Hasil Statistik:\n",
          "Chi-squared =", round(kw_result$statistic, 4), "\n",
          "df =", kw_result$parameter, "\n",
          "p-value =", format(kw_result$p.value, scientific = TRUE), "\n\n",
          "INTERPRETASI:\n", interpretation, "\n\n",
          "Median per Kelompok:\n", group_summary, "\n\n",
          "Kesimpulan:\n",
          if(kw_result$p.value < 0.05) {
            paste("Terdapat perbedaan yang signifikan dalam", input$kw_variable, 
                  "antar", tolower(grouping_var), ". Perlu analisis post-hoc untuk",
                  "mengetahui kelompok mana yang berbeda.")
          } else {
            paste("Tidak terdapat perbedaan yang signifikan dalam", input$kw_variable, 
                  "antar", tolower(grouping_var), ". Semua kelompok memiliki",
                  "distribusi yang relatif sama.")
          }
        )
      })
      
      output$kruskal_plot <- renderPlotly({
        p <- ggplot(df, aes_string(x = grouping_var, y = input$kw_variable, fill = grouping_var)) +
          geom_boxplot(alpha = 0.7, outlier.shape = NA) +
          geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
          stat_summary(fun = median, geom = "point", shape = 23, size = 3, 
                       fill = "white", color = "black") +
          labs(title = paste("Distribusi", input$kw_variable, "berdasarkan", grouping_var),
               subtitle = paste("p-value =", format(kw_result$p.value, digits = 4)),
               x = grouping_var, y = input$kw_variable) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "none",
                plot.title = element_text(size = 14, face = "bold"),
                plot.subtitle = element_text(size = 12, 
                                             color = ifelse(kw_result$p.value < 0.05, "red", "blue"))) +
          scale_fill_brewer(palette = "Set3")
        
        ggplotly(p, tooltip = c("x", "y"))
      })
      
    }, error = function(e) {
      output$kruskal_results <- renderText({
        paste("Error dalam analisis Kruskal-Wallis:", e$message, 
              "\nPastikan data memiliki variabilitas yang cukup dan kelompok yang valid.")
      })
    })
  })
  
  # Robust Regression (use current_data)
  observeEvent(input$run_regression, {
    df <- current_data()
    
    if (input$reg_year != "Semua") {
      df <- df[df$Tahun == input$reg_year, ]
    }
    if (input$reg_region != "Semua") {
      df <- df[df$Nama_Daerah == input$reg_region, ]
    }
    
    # Prepare formula
    formula_str <- paste(input$reg_dependent, "~", paste(input$reg_independent, collapse = " + "))
    
    # Fit robust regression model
    tryCatch({
      robust_model <- rlm(as.formula(formula_str), data = df, method = "M")
      values$regression_result <- robust_model
      
      output$regression_results <- renderText({
        model_summary <- summary(robust_model)
        
        coef_table <- data.frame(
          Variable = rownames(model_summary$coefficients),
          Estimate = round(model_summary$coefficients[,1], 4),
          Std_Error = round(model_summary$coefficients[,2], 4),
          t_value = round(model_summary$coefficients[,3], 4)
        )
        
        result_text <- paste(
          "Robust Regression Results\n",
          "=========================\n",
          "Model:", formula_str, "\n",
          "Sample size:", nrow(df), "\n",
          "Residual scale:", round(robust_model$s, 4), "\n\n",
          "Coefficients:\n"
        )
        
        for(i in 1:nrow(coef_table)) {
          result_text <- paste(result_text,
                               sprintf("%-20s: %8.4f (SE: %6.4f, t: %6.2f)\n", 
                                       coef_table$Variable[i], 
                                       coef_table$Estimate[i],
                                       coef_table$Std_Error[i],
                                       coef_table$t_value[i]))
        }
        
        result_text
      })
      
      # Residual plot
      output$regression_residual_plot <- renderPlotly({
        residuals_df <- data.frame(
          Fitted = fitted(robust_model),
          Residuals = residuals(robust_model)
        )
        
        p <- ggplot(residuals_df, aes(x = Fitted, y = Residuals)) +
          geom_point(alpha = 0.6, color = "#2E8B57") +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
          geom_smooth(method = "loess", se = FALSE, color = "#FF6B6B") +
          labs(title = "Residual Plot", x = "Fitted Values", y = "Residuals") +
          theme_minimal()
        
        ggplotly(p)
      })
      
      # Prediction vs Actual plot
      output$regression_prediction_plot <- renderPlotly({
        pred_df <- data.frame(
          Actual = df[[input$reg_dependent]],
          Predicted = fitted(robust_model)
        )
        
        p <- ggplot(pred_df, aes(x = Actual, y = Predicted)) +
          geom_point(alpha = 0.6, color = "#2E8B57") +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
          geom_smooth(method = "lm", se = TRUE, color = "#4ECDC4") +
          labs(title = "Predicted vs Actual", x = "Actual Values", y = "Predicted Values") +
          theme_minimal()
        
        ggplotly(p)
      })
      
    }, error = function(e) {
      output$regression_results <- renderText({
        paste("Error in regression analysis:", e$message)
      })
    })
  })
  
  # Time Series Analysis (use current_data) - TANPA FORECASTING
  observeEvent(input$run_timeseries, {
    df <- current_data()[current_data()$Nama_Daerah == input$ts_region, ]
    df <- df[order(df$Tahun, df$Bulan), ]
    
    # Check if we have enough data points
    if(nrow(df) < 12) {
      output$timeseries_plot <- renderPlotly({
        plot_ly() %>% 
          add_annotations(text = "Tidak cukup data untuk analisis time series (minimal 12 bulan)",
                          x = 0.5, y = 0.5, xref = "paper", yref = "paper")
      })
      return()
    }
    
    ts_data <- ts(df[[input$ts_variable]], start = c(min(df$Tahun), 1), frequency = 12)
    
    output$timeseries_plot <- renderPlotly({
      ts_df <- data.frame(
        Date = seq(as.Date(paste(min(df$Tahun), "-01-01", sep="")), by = "month", length.out = length(ts_data)),
        Value = as.numeric(ts_data)
      )
      
      p <- ggplot(ts_df, aes(x = Date, y = Value)) +
        geom_line(color = "#2E8B57", size = 1.2) +
        geom_point(color = "#FF6B6B", size = 1.5, alpha = 0.7) +
        labs(title = paste("Time Series:", input$ts_variable, "-", input$ts_region),
             x = "Tanggal", y = input$ts_variable) +
        theme_minimal() +
        theme(plot.title = element_text(color = "#1E6B4F", size = 14, face = "bold"))
      
      if (input$ts_trend) {
        p <- p + geom_smooth(method = "loess", color = "#4ECDC4", se = TRUE, alpha = 0.3)
      }
      
      ggplotly(p)
    })
    
    # Stationarity tests with detailed interpretation
    output$timeseries_tests <- renderText({
      tryCatch({
        # Augmented Dickey-Fuller test
        adf_test <- adf.test(ts_data)
        adf_result <- paste("ADF Test: p-value =", round(adf_test$p.value, 6))
        adf_interp <- ifelse(adf_test$p.value < 0.05, "STASIONER", "NON-STASIONER")
        
        # KPSS test
        kpss_test <- kpss.test(ts_data)
        kpss_result <- paste("KPSS Test: p-value =", round(kpss_test$p.value, 6))
        kpss_interp <- ifelse(kpss_test$p.value > 0.05, "STASIONER", "NON-STASIONER")
        
        # Overall conclusion
        overall_conclusion <- if(adf_interp == "STASIONER" && kpss_interp == "STASIONER") {
          "Data STASIONER - Dapat langsung dimodelkan"
        } else if(adf_interp == "NON-STASIONER" && kpss_interp == "NON-STASIONER") {
          "Data NON-STASIONER - Perlu differencing"
        } else {
          "Hasil uji berbeda - Perlu analisis lebih lanjut"
        }
        
        paste(
          adf_result, " → ", adf_interp, "\n",
          kpss_result, " → ", kpss_interp, "\n\n",
          "KESIMPULAN: ", overall_conclusion
        )
      }, error = function(e) {
        paste("Error dalam uji stasioneritas:", e$message)
      })
    })
    
    # ARIMA model with detailed interpretation
    output$timeseries_model <- renderText({
      tryCatch({
        auto_model <- auto.arima(ts_data, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
        
        # Extract model components
        p <- auto_model$arma[1]  # AR
        d <- auto_model$arma[6]  # I
        q <- auto_model$arma[2]  # MA
        P <- auto_model$arma[3]  # Seasonal AR
        D <- auto_model$arma[7]  # Seasonal I
        Q <- auto_model$arma[4]  # Seasonal MA
        
        seasonal_part <- if(P + D + Q > 0) {
          paste("(", P, ",", D, ",", Q, ")12", sep="")
        } else {
          ""
        }
        
        paste(
          "ARIMA(", p, ",", d, ",", q, ")", seasonal_part, "\n",
          "AIC = ", round(auto_model$aic, 2), "\n",
          "BIC = ", round(auto_model$bic, 2), "\n",
          "Sigma² = ", round(auto_model$sigma2, 6)
        )
      }, error = function(e) {
        paste("Error dalam modeling ARIMA:", e$message)
      })
    })
    
    # Model interpretation
    output$timeseries_interpretation <- renderText({
      tryCatch({
        auto_model <- auto.arima(ts_data, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
        
        # Analyze residuals
        residuals <- residuals(auto_model)
        ljung_test <- Box.test(residuals, type = "Ljung-Box")
        
        # Model adequacy
        model_adequacy <- ifelse(ljung_test$p.value > 0.05, 
                                 "Model ADEQUATE (residual white noise)",
                                 "Model perlu PERBAIKAN (residual ada pola)")
        
        # Trend analysis
        if(auto_model$arma[6] > 0) {  # d > 0
          trend_interpretation <- "Data memiliki TREND - telah di-differencing"
        } else {
          trend_interpretation <- "Data TIDAK memiliki trend"
        }
        
        # Seasonality analysis  
        if(auto_model$arma[7] > 0 || auto_model$arma[3] > 0 || auto_model$arma[4] > 0) {
          seasonal_interpretation <- "Data memiliki pola MUSIMAN"
        } else {
          seasonal_interpretation <- "Data TIDAK memiliki pola musiman"
        }
        
        paste(
          "INTERPRETASI MODEL:\n",
          "==================\n",
          trend_interpretation, "\n",
          seasonal_interpretation, "\n",
          "Adequacy: ", model_adequacy, "\n",
          "Ljung-Box p-value: ", round(ljung_test$p.value, 4), "\n\n",
          "REKOMENDASI:\n",
          if(ljung_test$p.value > 0.05) {
            "Model sudah baik untuk analisis temporal. Residual bersifat random."
          } else {
            "Model perlu diperbaiki. Pertimbangkan transformasi data atau model lain."
          }
        )
      }, error = function(e) {
        paste("Error dalam interpretasi model:", e$message)
      })
    })
  })
  
  # Spatial Visualization (Enhanced with statistical info) (use current_data)
  # Spatial Visualization (Enhanced with statistical info) (use current_data)
  observeEvent(input$update_map, {
    output$interactive_map <- renderLeaflet({
      df <- current_data()
      df <- df[df$Tahun == input$map_year & df$Bulan == input$map_month, ]
      
      # Cek apakah ada data untuk periode yang dipilih
      if(nrow(df) == 0) {
        return(leaflet() %>%
                 addProviderTiles(providers$CartoDB.Positron) %>%
                 setView(lng = 112.5, lat = -7.8, zoom = 8) %>%
                 addControl(
                   html = "<div style='background: red; color: white; padding: 10px; border-radius: 5px;'>
                    <b>Tidak ada data untuk periode yang dipilih</b><br>
                    Silakan pilih tahun dan bulan yang berbeda
                  </div>",
                   position = "topleft"
                 ))
      }
      
      # Aggregate data by region with statistics
      if (input$map_aggregation == "mean") {
        agg_data <- df %>% 
          group_by(Nama_Daerah) %>% 
          summarise(
            Value = mean(.data[[input$map_variable]], na.rm = TRUE),
            Count = n(),
            Min = min(.data[[input$map_variable]], na.rm = TRUE),
            Max = max(.data[[input$map_variable]], na.rm = TRUE),
            SD = sd(.data[[input$map_variable]], na.rm = TRUE),
            .groups = 'drop'
          )
        stat_type <- "Rata-rata"
      } else if (input$map_aggregation == "median") {
        agg_data <- df %>% 
          group_by(Nama_Daerah) %>% 
          summarise(
            Value = median(.data[[input$map_variable]], na.rm = TRUE),
            Count = n(),
            Min = min(.data[[input$map_variable]], na.rm = TRUE),
            Max = max(.data[[input$map_variable]], na.rm = TRUE),
            SD = sd(.data[[input$map_variable]], na.rm = TRUE),
            .groups = 'drop'
          )
        stat_type <- "Median"
      } else {
        agg_data <- df %>% 
          group_by(Nama_Daerah) %>% 
          summarise(
            Value = max(.data[[input$map_variable]], na.rm = TRUE),
            Count = n(),
            Min = min(.data[[input$map_variable]], na.rm = TRUE),
            Max = max(.data[[input$map_variable]], na.rm = TRUE),
            SD = sd(.data[[input$map_variable]], na.rm = TRUE),
            .groups = 'drop'
          )
        stat_type <- "Maksimum"
      }
      
      # Koordinat AKURAT untuk semua daerah Jawa Timur (berdasarkan koordinat resmi)
      coordinates <- data.frame(
        Nama_Daerah = c(
          # Kota-kota
          "Kota Batu", "Kota Blitar", "Kota Kediri", "Kota Madiun", 
          "Kota Malang", "Kota Mojokerto", "Kota Pasuruan", "Kota Probolinggo", 
          "Kota Surabaya",
          
          # Kabupaten-kabupaten (urutan alfabetis)
          "Kabupaten Bangkalan", "Kabupaten Banyuwangi", "Kabupaten Blitar", 
          "Kabupaten Bojonegoro", "Kabupaten Bondowoso", "Kabupaten Gresik",
          "Kabupaten Jember", "Kabupaten Jombang", "Kabupaten Kediri", 
          "Kabupaten Lamongan", "Kabupaten Lumajang", "Kabupaten Madiun", 
          "Kabupaten Magetan", "Kabupaten Malang", "Kabupaten Mojokerto", 
          "Kabupaten Nganjuk", "Kabupaten Ngawi", "Kabupaten Pacitan", 
          "Kabupaten Pamekasan", "Kabupaten Pasuruan", "Kabupaten Ponorogo", 
          "Kabupaten Probolinggo", "Kabupaten Sampang", "Kabupaten Sidoarjo", 
          "Kabupaten Situbondo", "Kabupaten Sumenep", "Kabupaten Trenggalek", 
          "Kabupaten Tuban", "Kabupaten Tulungagung"
        ),
        lat = c(
          # Kota-kota (lintang)
          -7.8740, -8.0983, -7.8481, -7.6298, 
          -7.9826, -7.4664, -7.6458, -7.7543, 
          -7.2575,
          
          # Kabupaten-kabupaten (lintang)
          -7.0378, -8.2325, -8.0983, 
          -7.1503, -7.9138, -7.1564,
          -8.1716, -7.5460, -7.8481, 
          -7.1194, -8.1335, -7.6298, 
          -7.6431, -8.0614, -7.4664, 
          -7.6052, -7.4039, -8.1996, 
          -7.0686, -7.6393, -7.8714, 
          -7.7543, -7.1869, -7.4378, 
          -7.7122, -7.0176, -8.0614, 
          -6.8997, -8.0614
        ),
        lng = c(
          # Kota-kota (bujur)
          112.5348, 112.1681, 112.0178, 111.5239, 
          112.6326, 112.4339, 112.9078, 113.2159, 
          112.7521,
          
          # Kabupaten-kabupaten (bujur)
          112.7378, 114.3691, 112.1681, 
          111.8814, 113.8213, 112.6544,
          113.7166, 112.2622, 112.0178, 
          112.4180, 113.2159, 111.5239, 
          111.3504, 112.6326, 112.4339, 
          111.9044, 111.4462, 111.0946, 
          113.4750, 112.9078, 111.4616, 
          113.2159, 112.9078, 112.7177, 
          113.9774, 113.8691, 111.7210, 
          111.8937, 111.9044
        ),
        stringsAsFactors = FALSE
      )
      
      # Debug: Print daerah yang tidak match
      missing_coords <- anti_join(agg_data, coordinates, by = "Nama_Daerah")
      missing_data <- anti_join(coordinates, agg_data, by = "Nama_Daerah")
      
      if(nrow(missing_coords) > 0) {
        cat("Daerah tanpa koordinat:\n")
        print(missing_coords$Nama_Daerah)
      }
      
      if(nrow(missing_data) > 0) {
        cat("Koordinat tanpa data:\n")  
        print(missing_data$Nama_Daerah)
      }
      
      # Gabungkan data dengan koordinat
      map_data <- inner_join(agg_data, coordinates, by = "Nama_Daerah")
      
      # Color palette based on data range
      pal <- colorNumeric(
        palette = c("#FFE5B4", "#FFCC99", "#FF9999", "#FF6666", "#FF3333", "#CC0000"),
        domain = map_data$Value,
        na.color = "#808080"
      )
      
      # Calculate overall statistics for interpretation
      overall_mean <- mean(map_data$Value, na.rm = TRUE)
      overall_max <- max(map_data$Value, na.rm = TRUE)
      overall_min <- min(map_data$Value, na.rm = TRUE)
      
      # Categorize regions
      high_regions <- sum(map_data$Value > (overall_mean + sd(map_data$Value, na.rm = TRUE)), na.rm = TRUE)
      low_regions <- sum(map_data$Value < (overall_mean - sd(map_data$Value, na.rm = TRUE)), na.rm = TRUE)
      
      leaflet(map_data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 112.5, lat = -7.8, zoom = 8) %>%
        addCircleMarkers(
          lng = ~lng, lat = ~lat,
          radius = ~sqrt(pmax(Value, 0)) * 5 + 5,  # Ensure positive radius
          color = "#FFFFFF",
          fillColor = ~pal(Value),
          fillOpacity = 0.8,
          weight = 2,
          popup = ~paste(
            "<div style='font-size: 14px;'>",
            "<b>", Nama_Daerah, "</b><br>",
            "<hr style='margin: 5px 0;'>",
            "<b>", input$map_variable, " (", stat_type, "):</b> ", round(Value, 3), "<br>",
            "<b>Range:</b> ", round(Min, 3), " - ", round(Max, 3), "<br>",
            "<b>Std Dev:</b> ", round(SD, 3), "<br>",
            "<b>Jumlah Data:</b> ", Count, "<br>",
            "<hr style='margin: 5px 0;'>",
            "<i>Ranking: ", 
            ifelse(Value > overall_mean + sd(map_data$Value, na.rm = TRUE), "Tinggi",
                   ifelse(Value < overall_mean - sd(map_data$Value, na.rm = TRUE), "Rendah", "Sedang")),
            "</i>",
            "</div>"
          ),
          label = ~paste(Nama_Daerah, ":", round(Value, 3))
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~Value,
          title = paste(input$map_variable, "<br>", stat_type),
          opacity = 1
        ) %>%
        addControl(
          html = paste(
            "<div style='background: white; padding: 10px; border-radius: 5px; box-shadow: 0 0 10px rgba(0,0,0,0.3); font-size: 12px;'>",
            "<b>Statistik Regional ", input$map_variable, "</b><br>",
            "<b>Periode:</b> ", month.name[input$map_month], " ", input$map_year, "<br>",
            "<b>Total Daerah:</b> ", nrow(map_data), "<br>",
            "<b>Rata-rata:</b> ", round(overall_mean, 3), "<br>",
            "<b>Range:</b> ", round(overall_min, 3), " - ", round(overall_max, 3), "<br>",
            "<b>Daerah Tinggi:</b> ", high_regions, " (", round(high_regions/nrow(map_data)*100, 1), "%)<br>",
            "<b>Daerah Rendah:</b> ", low_regions, " (", round(low_regions/nrow(map_data)*100, 1), "%)<br>",
            "<hr style='margin: 5px 0;'>",
            "<b>Interpretasi:</b><br>",
            ifelse(high_regions > nrow(map_data)*0.3, 
                   "Distribusi condong ke nilai tinggi", 
                   ifelse(low_regions > nrow(map_data)*0.3,
                          "Distribusi condong ke nilai rendah",
                          "Distribusi relatif merata")),
            "</div>"
          ),
          position = "topleft"
        )
    })
  })
  
  # Download handlers (simplified)
  output$download_raw_data <- downloadHandler(
    filename = function() {
      paste("climate_data_jatim_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(current_data(), file, row.names = FALSE)
    }
  )
  
  output$download_analysis <- downloadHandler(
    filename = function() {
      paste("analisis_hasil_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Combine all analysis results into one file
      analysis_results <- data.frame(
        Analysis_Type = character(),
        Variable_1 = character(),
        Variable_2 = character(),
        Statistic = character(),
        Value = numeric(),
        P_Value = numeric(),
        Interpretation = character(),
        stringsAsFactors = FALSE
      )
      
      # Add correlation results if available
      if (!is.null(values$correlation_result)) {
        corr_matrix <- values$correlation_result
        for(i in 1:nrow(corr_matrix)) {
          for(j in 1:ncol(corr_matrix)) {
            if(i != j) {
              analysis_results <- rbind(analysis_results, data.frame(
                Analysis_Type = "Spearman Correlation",
                Variable_1 = rownames(corr_matrix)[i],
                Variable_2 = colnames(corr_matrix)[j],
                Statistic = "Correlation Coefficient",
                Value = corr_matrix[i,j],
                P_Value = NA,
                Interpretation = ifelse(abs(corr_matrix[i,j]) > 0.7, "Strong", 
                                        ifelse(abs(corr_matrix[i,j]) > 0.3, "Moderate", "Weak"))
              ))
            }
          }
        }
      }
      
      # Add regression results if available
      if (!is.null(values$regression_result)) {
        reg_summary <- summary(values$regression_result)
        coef_table <- reg_summary$coefficients
        for(i in 1:nrow(coef_table)) {
          analysis_results <- rbind(analysis_results, data.frame(
            Analysis_Type = "Robust Regression",
            Variable_1 = rownames(coef_table)[i],
            Variable_2 = "Coefficient",
            Statistic = "Estimate",
            Value = coef_table[i, 1],
            P_Value = NA,
            Interpretation = ifelse(abs(coef_table[i, 3]) > 2, "Significant", "Not Significant")
          ))
        }
      }
      
      if(nrow(analysis_results) == 0) {
        analysis_results <- data.frame(
          Message = "Belum ada analisis yang dijalankan. Silakan jalankan analisis terlebih dahulu."
        )
      }
      
      write.csv(analysis_results, file, row.names = FALSE)
    }
  )
  
  output$download_plots <- downloadHandler(
    filename = function() {
      paste("grafik_analisis_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 12, height = 8)
      
      # Plot 1: Overview climate trends
      df_climate <- current_data() %>%
        group_by(Tahun) %>%
        summarise(
          Suhu = mean(Suhu, na.rm = TRUE),
          Curah_Hujan = mean(Curah_Hujan, na.rm = TRUE),
          Kelembapan = mean(Kelembapan, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        pivot_longer(cols = c("Suhu", "Curah_Hujan", "Kelembapan"), 
                     names_to = "Variable", values_to = "Value")
      
      p1 <- ggplot(df_climate, aes(x = Tahun, y = Value, color = Variable)) +
        geom_line(size = 1.5) + geom_point(size = 3) +
        facet_wrap(~Variable, scales = "free_y") +
        labs(title = "Tren Variabel Iklim Utama", x = "Tahun", y = "Nilai") +
        theme_minimal() + theme(legend.position = "none")
      print(p1)
      
      # Plot 2: Productivity distribution
      p2 <- ggplot(current_data(), aes(x = as.factor(Tahun), y = Produktivitas)) +
        geom_boxplot(fill = "#4ECDC4", alpha = 0.7) +
        geom_jitter(width = 0.2, alpha = 0.5) +
        labs(title = "Distribusi Produktivitas per Tahun", x = "Tahun", y = "Produktivitas") +
        theme_minimal()
      print(p2)
      
      # Plot 3: Correlation heatmap if available
      if (!is.null(values$correlation_result)) {
        corrplot(values$correlation_result, method = "color", type = "upper",
                 tl.col = "black", tl.srt = 45, addCoef.col = "black",
                 title = "Matriks Korelasi Spearman", mar = c(0,0,2,0))
      }
      
      # Plot 4: Scatter plot
      p4 <- ggplot(current_data(), aes(x = Suhu, y = Produktivitas)) +
        geom_point(aes(color = as.factor(Tahun)), alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE) +
        labs(title = "Hubungan Suhu vs Produktivitas", 
             x = "Suhu (°C)", y = "Produktivitas", color = "Tahun") +
        theme_minimal()
      print(p4)
      
      dev.off()
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
