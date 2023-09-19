package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.github.bohnman.squiggly.Squiggly;
import com.github.bohnman.squiggly.util.SquigglyUtils;

import org.apache.poi.ss.formula.functions.T;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

public class PartialFetchUtils {
    public static <T> Object fetchPartialData(ResponseEntity<RunnerResponse<T>> object, List<String> includeColumns) {

         if (includeColumns == null || includeColumns.size() == 0) {
            return object.getBody();
        }
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        ObjectMapper modified = Squiggly.init(objectMapper, String.join(",", includeColumns));
        String jsonString = SquigglyUtils.stringify(modified, object.getBody().getData());
        return (Object) jsonString;
    }

}
