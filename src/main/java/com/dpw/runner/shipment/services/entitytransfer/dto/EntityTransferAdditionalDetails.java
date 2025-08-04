package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.entity.enums.AirAuthorisingEntity;
import com.dpw.runner.shipment.services.entity.enums.AndesStatus;
import com.dpw.runner.shipment.services.entity.enums.LGDStatus;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferAdditionalDetails implements IEntityTranferBaseEntity {
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime customsNoIssueDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime expiryDate;

    private LocalDateTime blInstructionReceived;
    private String additionalSecurityInformation;
    private String regulatedEntityCategory;
    private AirAuthorisingEntity securityStatusReceivedFrom;
    private String exemptionCodes;
    private String aomFreeText;
    private String emergencyContactNumber;
    private String emergencyContactNumberCode;


    private String inspection;
    private String airwayBillDims;
    private BigDecimal shipperCOD;
    private String shipperCODPM;
    private String phase;
    private BigDecimal spotRate;
    private String spotRateType;
    private String efreightStatus;
    private Boolean importExportShipmentLock;
    public String CHAJobNumber;
    public String ADCode;
    public String BEType;
    private String customLocation;
    private String customCity;
    private Boolean isImportClearance;
    private Boolean isExportClearance;
    public String IGMFileNo;
    public String IECode;
    private String branchSINumber;
    private AndesStatus andesStatus;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime andesResponseDate;
    private String andesStatusResponseText;
    private String peruEntryExitPoint;
    private String tipoDocumentNotifyParty;
    private String tipoDocumentConsignee;
    private String tipoDocumentConsignor;
    private String andesTicket;
    private Long warehouseId;
    private String activityType;
    private Long hsnNumber;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    public LocalDateTime IGMFileDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    public LocalDateTime IGMInwardDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime inwardDateAndTime;
    private Long lineNumber;
    private Long subLineNumber;
    private Long localLineNumber;
    public String SMTPIGMNumber;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    public LocalDateTime SMTPIGMDate;
    private Boolean isInland;
    private Ownership ownership;
    private String ownershipName;
    private Ownership passedBy;
    private String passedByPerson;
    private LGDStatus lgdStatus;
    private Boolean isCmsHBLSent;
    private Boolean isCreditOverrideApproved;
    private BigDecimal freeDays;
    private String customHouse;
    private String supplierInvoiceNumber;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime supplierInvoiceDate;
    private BigDecimal invoiceValue;
    private BigDecimal assessValue;
    public BigDecimal CIFValue;
    private BigDecimal totalDuty;
    private String externalNotes;
    private Long bondedWarehouseId;
    private String releaseType;
    private String houseBillType;
    private String onBoard;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime onBoardDate;
    private String deliveryMode;
    private Integer original;
    private Integer copy;
    public String BLChargesDisplay;
    public String BLExporterShipment;
    private List<String> screeningStatus;
    private String paidPlace;
    private String placeOfIssue;
    private String placeOfSupply;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime dateOfIssue;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime dateOfReceipt;
    private String goodsCO;
    public String BOENumber;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    public LocalDateTime BOEDate;
    private Boolean printedOriginal;
    public Boolean WBLPrinted;
    private Boolean draftPrinted;
    private Boolean surrenderPrinted;
    private EntityTransferParties notifyParty;
    private EntityTransferParties importBroker;
    private EntityTransferParties exportBroker;
    private EntityTransferParties sendingForwarder;
    private EntityTransferParties receivingForwarder;
    private EntityTransferParties traderOrSupplier;
    private EntityTransferParties eTailor;
    private EntityTransferParties borrowedFrom;
    private EntityTransferParties sendingAgent;
    private EntityTransferParties receivingAgent;
    private String agentReference;
    private String sci;
    private Boolean emptyContainerReturned;
    private Map<String, EntityTransferMasterLists> masterData;
    private Map<String, EntityTransferUnLocations> unlocationData;
}
