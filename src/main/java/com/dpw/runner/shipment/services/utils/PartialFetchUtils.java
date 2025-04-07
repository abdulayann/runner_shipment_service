package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.github.bohnman.squiggly.Squiggly;
import com.github.bohnman.squiggly.util.SquigglyUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class PartialFetchUtils {

    @Autowired
    private JsonHelper jsonHelper;

    public <T> Object fetchPartialData(RunnerResponse<T> object, List<String> includeColumns) {

        if (includeColumns == null || includeColumns.isEmpty()) {
            return object;
        }
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        ObjectMapper modified = Squiggly.init(objectMapper, String.join(",", includeColumns));
        String jsonString = SquigglyUtils.stringify(modified, object.getData());
        return jsonHelper.readFromJson(jsonString,Object.class);
    }

    public <T> Object fetchPartialListData(IRunnerResponse object, List<String> includeColumns) {

        if (includeColumns == null || includeColumns.isEmpty()) {
            return object;
        }
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        ObjectMapper modified = Squiggly.init(objectMapper, String.join(",", includeColumns));
        String jsonString = SquigglyUtils.stringify(modified, object);
        return jsonHelper.readFromJson(jsonString, Object.class);
    }

}