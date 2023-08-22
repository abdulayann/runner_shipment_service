package com.dpw.runner.shipment.services.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class LoadRequest {
    private String load_uuid;
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
    private double weight;
    private String weight_uom;
    private int quantity;
    private String quantity_uom;
    private double volume;
    private String volume_uom;
    private DimensionDTO dimensions;

}
