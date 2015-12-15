/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package bikedatamapsinfo;

import java.lang.Exception;

import com.google.maps.GeoApiContext;
import com.google.maps.DistanceMatrixApiRequest;
import com.google.maps.model.Duration;
import com.google.maps.model.TravelMode;
import com.google.maps.model.DistanceMatrix;
import com.google.maps.model.DistanceMatrixRow;
import com.google.maps.model.DistanceMatrixElement;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 *
 * @author Matt
 */
public class BikeDataMapsInfo {

    // strings
    private static final String SECRET_KEY = "INSERTKEYHERE";
    private static final String BIKE_STATION_FILE = "HealthyRideStations2015.csv";
    private static final String OUTPUT_FILE_NAME = "transportation_times_"+new Date().getTime()+".csv";
    
    // data structures
    private static HashMap<Integer, Float[]> stationLocations = new HashMap<Integer, Float[]>();
    
    // load stations into memory
    public static void loadStations() {
        try {
            // open the file
            BufferedReader ourFile = new BufferedReader(new FileReader(
                    BIKE_STATION_FILE)); // Code snippet from
            // http://stackoverflow.com/questions/5488072/reading-in-from-system-in-java

            // discard the first line, which is the titles
            String textLine = ourFile.readLine();

            // read a line
            while ((textLine = ourFile.readLine()) != null) {
				// for each line, take the first part (station num) and 4th part
                // (latitude) and 5th part (longitude)
                String[] stationParts = textLine.split(",");

                // create float pair for lat/long
                Float[] latLong = new Float[2];
                latLong[0] = Float.parseFloat(stationParts[3]);
                latLong[1] = Float.parseFloat(stationParts[4]);

                // put it in the data structure
                stationLocations
                        .put(Integer.parseInt(stationParts[0]), latLong);

            }

            // close the file
            ourFile.close();

        } catch (FileNotFoundException fnfe) {
            System.err.println("Bike station file not found.");
        } catch (IOException ioe) {
            System.err.println("Bike station file IO error: "
                    + ioe.getMessage());
        }
    }
	

    public static void main(String[] args) {
        
        // set up the Google Maps API
        GeoApiContext mapsContext = new GeoApiContext();
        mapsContext.setApiKey(SECRET_KEY);
        DistanceMatrixApiRequest mapsRequest;
        TravelMode thisMode;
        
        // read in locations
        loadStations();
        
        // get type: bike, walk, transit (w/ times), cars (w/ times)
        // read from args: "biking", "walking", "transit", "driving"
        if (args.length > 0) {
            switch (args[0]) {
                case "biking": thisMode = TravelMode.BICYCLING;
                    break;
                case "walking": thisMode = TravelMode.WALKING;
                    break;
                case "transit": thisMode = TravelMode.TRANSIT;
                    break;
                case "driving": thisMode = TravelMode.DRIVING;
                    break;
                default: thisMode = TravelMode.BICYCLING;
                    break;
            }
        } else {
            // default to biking
            thisMode = TravelMode.BICYCLING;
        }

        try {
            // open output file
            PrintWriter outputter = new PrintWriter(new BufferedWriter(new FileWriter(OUTPUT_FILE_NAME)));

            // for each pair (do bidirectionally)
            Iterator outerIt = stationLocations.entrySet().iterator();
            while (outerIt.hasNext()) {
                Map.Entry<Integer, Float[]> outerStation = (Map.Entry)outerIt.next();
                Iterator innerIt = stationLocations.entrySet().iterator();
                while (innerIt.hasNext()) {
                    Map.Entry<Integer, Float[]> innerStation = (Map.Entry)innerIt.next();
			    
                    // set up request
                    mapsRequest = new DistanceMatrixApiRequest(mapsContext);
                    mapsRequest.origins(outerStation.getValue()[0] + "," + outerStation.getValue()[1])
                                .destinations(innerStation.getValue()[0] + "," + innerStation.getValue()[1])
                            .mode(thisMode);
                    // IF DRIVING OR TRANSIT, WE NEED TIME
                    if (thisMode.equals(TravelMode.DRIVING) || thisMode.equals(TravelMode.TRANSIT)) {
                        throw new Exception("YOU HAVEN'T SET UP THE DRIVING OR TRANSIT TIMING INFO, SILLY");
                    }

                    // make the request
                    DistanceMatrix ourResult = mapsRequest.await();

                    // read response
                    if (ourResult.rows.length == 0) {
                        // if there are no rows in the result, we have an issue
                        throw(new Exception("No rows in maps result!"));
                    } else {
                        // we care about the first row
                        DistanceMatrixRow ourRow = ourResult.rows[0];
                        DistanceMatrixElement[] ourElement = ourRow.elements;

                        if (ourElement.length == 0) {
                        // if there are no elements in the result, we have an issue
                        throw(new Exception("No elements in maps result!"));
                        } else {
                            // get our duration!
                            Duration aDuration = ourElement[0].duration;

                            // add to our output file
                            outputter.println(outerStation.getKey() + "," + innerStation.getKey() + "," + aDuration.inSeconds);
                        }
                    }
                }
            }
         
            // close output file
            outputter.flush();
            outputter.close();
            
        } catch (Exception e) {
                System.err.println(e.toString());
        }

    }
    
}
