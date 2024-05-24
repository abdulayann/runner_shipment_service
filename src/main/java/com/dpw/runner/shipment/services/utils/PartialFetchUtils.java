package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.github.bohnman.squiggly.Squiggly;
import com.github.bohnman.squiggly.util.SquigglyUtils;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class PartialFetchUtils {

    private static JsonHelper jsonHelper;

    public PartialFetchUtils(JsonHelper jsonHelper){
        this.jsonHelper=jsonHelper;
    }
    public static <T> Object fetchPartialData(RunnerResponse<T> object, List<String> includeColumns) {

         if (includeColumns == null || includeColumns.size() == 0) {
            return object;
        }
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        ObjectMapper modified = Squiggly.init(objectMapper, String.join(",", includeColumns));
        String jsonString = SquigglyUtils.stringify(modified, object.getData());
        return jsonHelper.readFromJson(jsonString,Object.class);
    }

    public static <T> Object  fetchPartialListData(IRunnerResponse object, List<String> includeColumns) {

        if (includeColumns == null || includeColumns.size() == 0) {
            return object;
        }
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        ObjectMapper modified = Squiggly.init(objectMapper, String.join(",", includeColumns));
        String jsonString = SquigglyUtils.stringify(modified, object);
        return jsonHelper.readFromJson(jsonString, Object.class);
    }

}
