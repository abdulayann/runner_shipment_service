package com.dpw.runner.shipment.services.dto.request.npm;

import lombok.Data;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;

@Data
@SuppressWarnings("java:S1948")
public class NPMOffer implements Serializable {
    public Double total_route_price;
    public Double total_route_cost;
    public Long ID;
    public List<ChargeGroups> charge_groups;
    public List<EntityRateCards> entity_rate_cards;
    public Double chargeable;
    public String chargeable_uom;

    @Data
    public static class  ChargeGroups implements Serializable{
        public String group_name;
        public String group_sell_rate;
        public Long ID;
        public List<ChargeCodes> charge_codes;
    }

    @Data
    public static class ChargeCodes implements Serializable{
        public String carrier;
        public String rate_name;
        public Long ID;
        public String rate_type;
        public String price;
        public String base_price_currency;
        public String conversion_rate;
        public String required_currency;
        public Boolean applicable_on_booking;
    }

    @Data
    public static class EntityRateCards implements Serializable{
        public Double entity_price;
        public Long ID;
        public String node;
        public String origin;
        public String destination;
        public String type;
        public String carrier;
        public String charge_code;
        public String rates_uom;
        public String currency_type;
        public List<LoadsRatesInfo> loads_rates_info;
        public List<LoadsRatesInfo> aggregated_shipment_load_rates_info;
    }

    @Data
    public static class LoadsRatesInfo implements Serializable{
        public String load_type;
        public Long ID;
        public Double load_price;
        public String cargo_type;
        public Long quantity;
        public Double gross_volume;
        public Double gross_weight;
        public Double volumetric_weight;
        public Double chargeable_weight;
        public String weight_uom;
        public String volume_uom;
        public String product_category_code;
        public List<AssociatedRate> associated_rates;
        public Double weight;
    }

    @Data
    public static class AssociatedRate implements Serializable{
        public Long ID;
        public String carrier;
        public String rate_name;
        public String rates_uom;
        public String rate_type;
        public Double calculated_sell;
        public Double calculated_cost;
        public Double procured_sell;
        public Double procured_cost;
        public String base_price_currency;
        public String required_currency;
        public Double conversion_rate;
        public Boolean applicable_on_booking;
        public String sourceName;
        public LocalDateTime uploadedOn;
        public String packId;
        public String packType;
        public SourceDetails source_details;
        public Double rate;
        public Double unit_cost;
        public Double unit_sell;
        public String charge_basis;
        public List<SlabRate> slabs;
        public Double minimum_price;
        public Double maximum_price;
        public Double load_rate_weight;
        public String service_category;

        public Double chargeable;

        public String chargeable_uom;
    }

    @Data
    public static class SlabRate implements Serializable{
        public Long ID;
        public Double slab_low;
        public Double slab_high;
        public String slab_type;
        public Double slab_sell;
        public Double slab_cost;
        public String value_type;
        public Double min_fare;
        public Double max_fare;
    }

    @Data
    public static class SourceDetails implements Serializable{
        public String _id;
        public String source_id;
        public String source_name;
        public String uploaded_email_ids;
        public LocalDateTime uploaded_on;
        public String pack_id;
        public String pack_type;
    }


}
