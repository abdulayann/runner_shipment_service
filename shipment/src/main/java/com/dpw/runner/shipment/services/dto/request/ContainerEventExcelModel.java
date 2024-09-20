package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.utils.ExcelCell;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ContainerEventExcelModel {
    @ExcelCell(displayName = "ContainerNumber", order = 1)
    private String containerNumber;
    @ExcelCell(displayName = "EventCode", order = 2)
    private String eventCode;
    @ExcelCell(displayName = "Estimated", order = 3)
    private LocalDateTime estimated;
    @ExcelCell(displayName = "Actual", order = 4)
    private LocalDateTime actual;
    @ExcelCell(displayName = "PlaceName", order = 5)
    private String placeName;
    @ExcelCell(displayName = "PlaceDescription", order = 6)
    private String placeDescription;
    @ExcelCell(displayName = "Latitude", order = 7)
    private String latitude;
    @ExcelCell(displayName = "Longitude", order = 8)
    private String longitude;
    @ExcelCell(displayName = "Source", order = 9)
    private String source;
    @ExcelCell(displayName = "PublicTrackingEvent", order = 10)
    private Boolean isPublicTrackingEvent;
}
