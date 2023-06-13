package com.dpw.runner.shipment.services.utility;

import com.dpw.runner.shipment.services.dto.request.Criteria;
import com.dpw.runner.shipment.services.dto.request.FilterCriteria;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.*;

import static com.dpw.runner.shipment.services.utility.Constants.*;

public class CommonUtils {

    private static final Logger LOG = LoggerFactory.getLogger(CommonUtils.class);
    private static final String resourcePath = String.format("%s%s",System.getProperty("user.dir"),"/src/main/resources/");
    private static ObjectMapper mapper = new ObjectMapper().enable(SerializationFeature.INDENT_OUTPUT);

    public static List<Object> getAclPermissionList(){
        File jsonInputFile = new File(String.format("%s%s",resourcePath,"acl.json"));
        List<Object> jsonData = new ArrayList<>();
        Map<String, Object> obj = new HashMap<>();
        InputStream is;
        LOG.info("Loaded input file : {}", jsonInputFile);

        try{
            is = new FileInputStream(jsonInputFile);
            JsonReader reader = Json.createReader(is);
            JsonArray permission = reader.readArray();
            reader.close();
            for (int i = 0; i < permission.size(); i++) {
                JsonObject temp = permission.getJsonObject(i);
//                LOG.info("index {}", i);
                obj.put(temp.get("transportMode").toString(),temp.get("direction"));
            }

//            LOG.info(mapper.writeValueAsString(obj));
            jsonData = Arrays.asList(permission.toArray());
            LOG.info("Total permissions scanned : {}", permission.size());
        } catch(Exception e) {
            LOG.error("Failed to read input json file", e);
        }
        return jsonData;
    }

    public static JsonObject readJson(String FilePath){
        File jsonInputFile = new File(FilePath);
        JsonObject jsonData = null;
        Map<String, Object> obj = new HashMap<>();
        InputStream is;
        LOG.info("Loaded input file : {}", jsonInputFile);

        try {
            is = new FileInputStream(jsonInputFile);
            JsonReader reader = Json.createReader(is);
            JsonObject permissions = reader.readObject();
            jsonData = permissions;
            reader.close();
        } catch (Exception e){
            LOG.error("Failed to read input json file", e);
        }

        return jsonData;
    }

    public static List<FilterCriteria> generateFilterCriteriaFromPermissionType(String permissionType, HashSet<String> permissionSet){
        List<FilterCriteria> innerFilters = new ArrayList();
        JsonObject criteriaObj = readJson(String.format("%s%s",resourcePath,"permissions.json")).getJsonObject(permissionType);

        if(criteriaObj.get(TRANSPORT_MODE).toString() != ALL)
            innerFilters.add(constructCriteria(TRANSPORT_MODE, criteriaObj.get(TRANSPORT_MODE).toString(),"=" , null));
        if(criteriaObj.get(DIRECTION).toString() != ALL)
            innerFilters.add(constructCriteria(DIRECTION, criteriaObj.get(DIRECTION).toString(),"=" , "and"));
        if(criteriaObj.get(SHIPMENT_TYPE).toString() != ALL)
            innerFilters.add(constructCriteria(SHIPMENT_TYPE, criteriaObj.get(SHIPMENT_TYPE).toString(),"=" , "and"));

        return innerFilters;
    }

    private static FilterCriteria constructCriteria(String fieldName, Object value, String operator, String logicalOperator) {
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        return FilterCriteria.builder().criteria(criteria).logicOperator(logicalOperator).build();
    }

}
