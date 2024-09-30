package com.dpw.runner.booking.services.dto.request.npm;

import com.dpw.runner.booking.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class HazardousInfoRequest implements IRunnerRequest {
    private Boolean is_hazardous;
    private List<ClassificationInfoRequest> classification_info;
}
