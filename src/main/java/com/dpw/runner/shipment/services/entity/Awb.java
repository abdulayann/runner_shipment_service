package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.dto.request.awb.*;
import com.dpw.runner.shipment.services.entity.enums.AwbStatus;
import com.dpw.runner.shipment.services.entity.enums.PrintType;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.JdbcTypeCode;
import jakarta.persistence.*;
import org.hibernate.type.SqlTypes;

import java.time.LocalDateTime;
import java.util.List;

@Entity
@Setter
@Getter
@Table(name = "awb")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Accessors(chain = true)
@JsonIgnoreProperties(ignoreUnknown = true)
public class Awb extends MultiTenancy {
    @Column(name = "shipment_id")
    private Long shipmentId;

    @Column(name = "awb_number")
    private String awbNumber;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "awb_shipment_info", columnDefinition = "jsonb")
    private AwbShipmentInfo awbShipmentInfo;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "awb_notify_party_info", columnDefinition = "jsonb")
    private List<AwbNotifyPartyInfo> awbNotifyPartyInfo;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "awb_routing_info", columnDefinition = "jsonb")
    private List<AwbRoutingInfo> awbRoutingInfo;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "awb_cargo_info", columnDefinition = "jsonb")
    private AwbCargoInfo awbCargoInfo;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "air_messaging_additional_fields", columnDefinition = "jsonb")
    private AirMessagingAdditionalFields airMessagingAdditionalFields;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "awb_payment_info", columnDefinition = "jsonb")
    private AwbPaymentInfo awbPaymentInfo;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "awb_other_charges_info", columnDefinition = "jsonb")
    private List<AwbOtherChargesInfo> awbOtherChargesInfo;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "awb_other_info", columnDefinition = "jsonb")
    private AwbOtherInfo awbOtherInfo;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "awb_oci_info", columnDefinition = "jsonb")
    private List<AwbOCIInfo> awbOciInfo;

    @Column(name = "acas_enabled")
    private Boolean acasEnabled;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "oci_info", columnDefinition = "jsonb")
    private OCIInfo ociInfo;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "awb_goods_description_info", columnDefinition = "jsonb")
    private List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfo;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "awb_packing_info", columnDefinition = "jsonb")
    private List<AwbPackingInfo> awbPackingInfo;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "awb_special_handling_codes_Mappings", columnDefinition = "jsonb")
    private List<AwbSpecialHandlingCodesMappingInfo> awbSpecialHandlingCodesMappings;

    @Column(name = "consolidation_id")
    private Long consolidationId;

    @Enumerated(EnumType.STRING)
    @Column(name = "air_message_status")
    private AwbStatus airMessageStatus;

    @Enumerated(EnumType.STRING)
    @Column(name = "linked_hawb_air_message_status")
    private AwbStatus linkedHawbAirMessageStatus;

    @Column(name = "user_mail_id")
    private String userMailId;

    @Column(name = "user_display_name")
    private String userDisplayName;

    @Column(name = "air_message_resubmitted")
    private Boolean airMessageResubmitted;

    @Enumerated(EnumType.STRING)
    @Column(name = "print_type")
    private PrintType printType;

    @Column(name = "original_printed_at")
    private LocalDateTime originalPrintedAt;
}
