package com.dpw.runner.shipment.services.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.UUID;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class LoadRequest {
    private UUID load_uuid;
    private String load_type;
    private String container_type_code;
    private String pkg_type;
    private String product_category_code;
    private String product_name;
    private String pickup_date;
    private String sailing_date;
    private boolean is_odc;
    private boolean is_reefer;
    private ReeferInfoRequest reefer_info;
    private boolean is_package;
    private boolean is_hazardous;
    private HazardousInfoRequest hazardous_info;
    private BigDecimal weight;
    private String weight_uom;
    private Long quantity;
    private String quantity_uom;
    private BigDecimal volume;
    private String volume_uom;
    private DimensionDTO dimensions;
}
