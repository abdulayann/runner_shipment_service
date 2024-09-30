package com.dpw.runner.booking.services.dto.response;

import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import lombok.Data;
import java.util.UUID;

@Data
public class DefaultViewsResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long defaultViewId;
    private String username;
    private String entity;
}
