package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import lombok.Builder;
import lombok.Data;

import java.util.Map;

/**
 * Data transfer object to pass preparation data between report generation phases.
 * This avoids passing multiple parameters and keeps data flow clear.
 */
@Data
@Builder
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
    
    // For combined reports (early returns)
    private boolean isCombinedReport;
    private byte[] combinedReportBytes;
    
    /**
     * Factory method for combined report data
     */
    public static ReportPreparationData forCombinedReport(byte[] bytes) {
        return ReportPreparationData.builder()
                .isCombinedReport(true)
                .combinedReportBytes(bytes)
                .build();
    }
}
