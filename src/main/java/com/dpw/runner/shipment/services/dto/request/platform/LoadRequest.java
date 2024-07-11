package com.dpw.runner.shipment.services.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.UUID;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class LoadRequest implements Serializable {
    private UUID load_uuid;
    private String load_type;
    private String container_type_code;
    private String pkg_type;
    private String product_category_code;
    private String product_name;
    private String pickup_date;
    private String sailing_date;
    private Boolean is_odc;
    private Boolean is_reefer;
    private ReeferInfoRequest reefer_info;
    private Boolean is_package;
    private Boolean is_hazardous;
    private HazardousInfoRequest hazardous_info;
    private BigDecimal weight;
    private String weight_uom;
    private Long quantity;
    private String quantity_uom;
    private BigDecimal volume;
    private String volume_uom;
    private DimensionDTO dimensions;
}
