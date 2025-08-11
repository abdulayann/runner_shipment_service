package com.dpw.runner.shipment.services.dto.shipment_console_dtos;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ShipmentSummaryWarningsResponse implements IRunnerResponse {

    private WarningDetail packagesWarning;
    private WarningDetail weightWarning;
    private WarningDetail volumeWarning;

    @Builder
    @Getter
    @Setter
    public static class WarningDetail {
        private Boolean showWarning;
        private String containerValue;
        private String packageValue;
        private String difference;

        public WarningDetail() {}

        public WarningDetail(boolean showWarning, String containerValue, String packageValue, String difference) {
            this.showWarning = showWarning;
            this.containerValue = containerValue;
            this.packageValue = packageValue;
            this.difference = difference;
        }
    }
}

