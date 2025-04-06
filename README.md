# Symptom Checker

A Haskell web service that helps users check their symptoms against a database of common conditions.

## Features

- RESTful API endpoints
- JSON-based communication
- CORS-enabled for web client integration
- Pre-defined database of common conditions and their symptoms
- Results sorted by number of matching symptoms
- Modern web interface

## Requirements

- GHC (Glasgow Haskell Compiler) version 4.17.0.0 or later
- Cabal (Haskell package manager)
- Node.js and npm (for the frontend)

## Installation

1. Clone this repository
2. Install Haskell dependencies:
   ```
   cabal update
   cabal build
   ```

3. Install frontend dependencies:
   ```
   cd public
   npm install
   ```

## Running the Application

The application consists of two components: a Haskell backend server and a Node.js frontend server.

1. Start the backend server (from the project root):
   ```
   cabal run symptom-checker
   ```
   The backend server will start on port 3000

2. Start the frontend server (in a new terminal):
   ```
   cd public
   npm start
   ```
   The frontend server will start on port 8080

3. Open your web browser and navigate to:
   ```
   http://localhost:8080
   ```

## Project Structure

### Backend Files (`src/`)

- `Main.hs`: The main entry point for the backend server. Handles HTTP routing, request processing, and server configuration.
- `SymptomChecker.hs`: Contains the core symptom checking logic, including the database of conditions and their symptoms.

### Frontend Files (`public/`)

- `index.html`: The main web interface file containing the HTML structure and styling.
- `server.js`: A simple Express.js server that serves the static frontend files.
- `package.json`: Node.js project configuration and dependency management.
- `package-lock.json`: Detailed dependency tree for npm packages.

## API Endpoints

### Check Symptoms
- **Endpoint**: `POST /api/check`
- **Request Body**:
  ```json
  {
    "symptoms": ["fever", "cough", "sore throat"]
  }
  ```
- **Response**:
  ```json
  {
    "conditions": [
      ["Flu", 3],
      ["Common Cold", 2]
    ]
  }
  ```

### Get Available Symptoms
- **Endpoint**: `GET /api/symptoms`
- **Response**:
  ```json
  {
    "symptoms": ["fever", "cough", "sore throat", ...]
  }
  ```

## Development

- The backend code is in the `src` directory
- The frontend code is in the `public` directory
- The backend uses the following main dependencies:
  - warp (web server)
  - aeson (JSON handling)
  - wai (web application interface)
  - wai-cors (CORS support)

## Note

This is a simple educational project and should not be used as a substitute for professional medical advice. Always consult with a healthcare provider for proper medical diagnosis and treatment. 