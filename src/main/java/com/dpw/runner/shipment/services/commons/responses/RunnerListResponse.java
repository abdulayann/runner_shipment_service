package com.dpw.runner.shipment.services.commons.responses;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * /**
 * * Generic List response.
 */
@SuppressWarnings("rawtypes")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RunnerListResponse<T extends IRunnerResponse> implements IRunnerResponse {
    private boolean success;
    private String requestId;
    private List<? extends T> data;
    private int totalPages;
    private long numberOfRecords;
    private ApiError error;

    public List<T> getData(Class clazz) {
        ObjectMapper objectMapper = new ObjectMapper();
        return objectMapper.convertValue(data, objectMapper.getTypeFactory().constructCollectionType(List.class, clazz));
    }
}
