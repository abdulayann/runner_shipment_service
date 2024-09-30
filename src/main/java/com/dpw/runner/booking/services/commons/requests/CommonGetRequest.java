package com.dpw.runner.booking.services.commons.requests;

import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class CommonGetRequest implements IRunnerRequest {
    private Long id;
    private String guid;
    private List<String>includeColumns;
}
