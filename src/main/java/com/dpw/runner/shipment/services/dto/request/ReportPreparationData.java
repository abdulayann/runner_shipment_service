package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import lombok.Builder;
import lombok.Data;

import java.util.Map;

@Builder
@Data
public class ReportPreparationData {
    private ReportRequest reportRequest;
    private ShipmentSettingsDetails tenantSettingsRow;
    private Boolean isOriginalPrint;
    private Boolean isSurrenderPrint;
    private Boolean isNeutralPrint;
    private IReport report;
    private Awb awb;
    private Map<String, Object> dataRetrieved;
    private String hbltype;
    private String objectType;
    private boolean combinedReport;
    private byte[] combinedReportBytes;

    public static ReportPreparationData forCombinedReport(byte[] bytes) {
        return ReportPreparationData.builder()
                .combinedReport(true)
                .combinedReportBytes(bytes)
                .build();
    }
}
