package com.ibm.arc.testhealth;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;

import org.apache.http.HttpResponse;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.json.JSONException;
import org.json.JSONObject;
/**
 * 
 * @author cpawan
 *
 */
public class TestRESTClientForHealth {
	
	public static void main (String [] argb)
	{
		try {
			testTKROutCome();
		} catch (IllegalStateException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * 
	 * @throws IllegalStateException
	 * @throws IOException
	 * @throws JSONException
	 */
	public static void testTKROutCome() throws IllegalStateException, IOException, JSONException {
		
	// prepare data for test
		JSONObject json = new JSONObject();
		json.put("userID","TestUser112");
		json.put("age",62);
		json.put("gender","M");
		json.put("weight",190.5);
		json.put("height", 67);
		json.put("bmi",34);
		
		
		JSONObject jsonQ = new JSONObject();
//		jsonQ.put("isPreOpAntiBiotics",1);
//		jsonQ.put("isInNetworkOrthopedic",1);
		jsonQ.put("isSmoker",1);
		jsonQ.put("isDiabetes",1);
		jsonQ.put("isAnaemia",1);
		jsonQ.put("isMentalDisorder",0);
//		jsonQ.put("isNervousSystemDisorder",1);
//		jsonQ.put("isHyperTension",1);
//		jsonQ.put("isRespiratoryCondition",1);
//		jsonQ.put("isRheumatism",1);
		
		json.put("surveyData", jsonQ);
	
	// print to make sure the data being sent is in right format.	
		System.out.println(json.toString());
		
	// server side REST end point	
		String url = "http://scan5.almaden.ibm.com:9080/HealthAnalyticsServicesA1/api/healthAnalytics";
		
	// method that makes http call
		String outcome = TestRESTClientForHealth.callServerURL(url, json.toString() );
		System.out.println("Health outcome ...... \n" + outcome);
	}
	
	/**
	 * 
	 * @param url
	 * @param inputEntity
	 * @return JSON health outcome
	 * @throws IllegalStateException
	 * @throws IOException
	 */
	public static String callServerURL(String url, String inputEntity)
			throws IllegalStateException, IOException {

		HttpClient client = new DefaultHttpClient();

		// example: url = "http://localhost:9080/eHelpRoomWS/api/room/save"
		HttpPost post = new HttpPost(url);

		StringEntity input = null;
		try {
			input = new StringEntity(inputEntity);
			System.out.println(input);

		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		input.setContentType("application/json");
		post.setEntity(input);

		HttpResponse response = null;
		try {
			response = client.execute(post);
		} catch (ClientProtocolException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		BufferedReader rd = new BufferedReader(new InputStreamReader(response
				.getEntity().getContent()));

		String line = "";

		// at present just printing
		StringBuffer str = new StringBuffer();
		while ((line = rd.readLine()) != null) {
			str.append(line);
			//System.out.println(line);
		}
		rd.close();
		return str.toString();
	}
}
