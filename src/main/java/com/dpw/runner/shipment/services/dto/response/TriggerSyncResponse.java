package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.List;

@Data
@Builder
@ApiModel("Trigger Sync Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TriggerSyncResponse implements IRunnerResponse {
    private List<SyncResponse> succeeded;
    private List<SyncResponse> failed;

    @Data
    @Builder
    public static class SyncResponse implements IRunnerResponse {
        private Long id;
        private String moduleType;
        private String moduleId;
        private String reason;
    }

}





