package com.dpw.runner.shipment.services.utils;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ADDRESS1;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CITY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.COMM;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.COMPANY_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONTAINER_COUNT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DG_CONTAINER_COUNT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ETA;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ETA_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ETD;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ETD_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.LIST_ALL_DOCUMENTS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.STATE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SUMMARY_DOCUMENTS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.VESSEL_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ZIP_POST_CODE;
import static com.dpw.runner.shipment.services.commons.constants.CacheConstants.CARRIER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ACTIONED_USER_NAME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.AIR_SECURITY_PERMISSION_MSG;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ALLOCATED_VOLUME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ALLOCATED_VOLUME_UNIT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ALLOCATED_WEIGHT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ALLOCATED_WEIGHT_UNIT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.APPROVED_TIME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.APPROVER_NAME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.AUTO_REJECTION_REMARK;
import static com.dpw.runner.shipment.services.commons.constants.Constants.BRANCH_TIME_ZONE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARGO_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARGO_TYPE_FCL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARGO_TYPE_FTL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARGO_TYPE_LCL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARGO_TYPE_LTL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARRIER_CODE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARRIER_NAME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.COMMERCIAL_OCEAN_DG_ROLE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSOLIDATION_CREATE_USER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSOL_BRANCH_CODE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSOL_BRANCH_NAME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CROSS_TENANT_SOURCE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CUSTOMER_BOOKING;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DATE_TIME_FORMAT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DESTINATION_PORT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DG_APPROVER_NAME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DG_APPROVER_TIME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DG_OCEAN_APPROVAL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DG_PACKAGES_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DIRECTION_EXP;
import static com.dpw.runner.shipment.services.commons.constants.Constants.EMPTY_STRING;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ERROR_WHILE_SENDING_EMAIL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.FLIGHT_NUMBER1;
import static com.dpw.runner.shipment.services.commons.constants.Constants.HAWB_NUMBER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.HTML_HREF_TAG_PREFIX;
import static com.dpw.runner.shipment.services.commons.constants.Constants.HTML_HREF_TAG_SUFFIX;
import static com.dpw.runner.shipment.services.commons.constants.Constants.HUB_BRANCH_CODE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.HUB_BRANCH_NAME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ID;
import static com.dpw.runner.shipment.services.commons.constants.Constants.INTERBRANCH_CONSOLIDATION_NUMBER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.INTERBRANCH_CONSOLIDATION_NUMBER_WITHOUT_LINK;
import static com.dpw.runner.shipment.services.commons.constants.Constants.INTERBRANCH_SHIPMENT_NUMBER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.INTERBRANCH_SHIPMENT_NUMBER_WITHOUT_LINK;
import static com.dpw.runner.shipment.services.commons.constants.Constants.LAT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.MAWB_NUMBER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.NETWORK_TRANSFER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.OCEAN_DG_APPROVAL_APPROVE_EMAIL_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.OCEAN_DG_APPROVAL_REJECTION_EMAIL_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.OCEAN_DG_APPROVAL_REQUEST_EMAIL_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.OCEAN_DG_COMMERCIAL_APPROVAL_APPROVE_EMAIL_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.OCEAN_DG_COMMERCIAL_APPROVAL_REJECTION_EMAIL_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.OCEAN_DG_COMMERCIAL_APPROVAL_REQUEST_EMAIL_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.OCEAN_DG_ROLE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.OCEAN_DG_TASKTYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ORIGIN_PORT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.PENDING_ACTION;
import static com.dpw.runner.shipment.services.commons.constants.Constants.PENDING_ACTION_TASK;
import static com.dpw.runner.shipment.services.commons.constants.Constants.POD_NAME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.POL_NAME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.REGIONAL_BRANCH_CODE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.REGIONAL_BRANCH_NAME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.REMARKS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.REQUESTED_USER_NAME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.REQUESTER_REMARKS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.REQUEST_DATE_TIME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENTS_WITH_SQ_BRACKETS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_ASSIGNED_USER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_ASSIGNED_USER_WITH_SLASH;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_BRANCH_CODE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_BRANCH_NAME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_CREATE_USER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_DETACH_EMAIL_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_DETAILS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_NUMBER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_PULL_ACCEPTED_EMAIL_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_PULL_REJECTED_EMAIL_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_PULL_REQUESTED_EMAIL_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_PULL_WITHDRAW_EMAIL_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_PUSH_ACCEPTED_EMAIL_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_PUSH_REJECTED_EMAIL_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_PUSH_REQUESTED_EMAIL_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_PUSH_WITHDRAW_EMAIL_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_VOLUME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_VOLUME_UNIT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_WEIGHT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_WEIGHT_UNIT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SOURCE_CONSOLIDATION_NUMBER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.STATUS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TENANTID;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TIME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TOTAL_PACKAGES_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_AIR;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_RAI;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_ROA;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_SEA;
import static com.dpw.runner.shipment.services.commons.constants.Constants.UAE_TWO_DIGIT_IATA_CODE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.USERNAME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.USER_BRANCH;
import static com.dpw.runner.shipment.services.commons.constants.Constants.USER_COUNTRY;
import static com.dpw.runner.shipment.services.commons.constants.Constants.USER_NAME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.VIEWS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.VOLUME_UNIT_M3;
import static com.dpw.runner.shipment.services.commons.constants.Constants.VOYAGE;
import static com.dpw.runner.shipment.services.commons.constants.PermissionConstants.CAN_VIEW_ALL_BRANCH_SHIPMENTS;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_ACCEPTED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_COMMERCIAL_ACCEPTED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_COMMERCIAL_REJECTED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_COMMERCIAL_REQUESTED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_REJECTED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_REQUESTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_DETACH;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PULL_ACCEPTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PULL_REJECTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PULL_REQUESTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PULL_WITHDRAW;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PUSH_ACCEPTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PUSH_REJECTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PUSH_WITHDRAW;
import static com.dpw.runner.shipment.services.utils.CountryListHelper.ISO3166.getAlpha3FromAlpha2;
import static com.dpw.runner.shipment.services.utils.DateUtils.convertDateToUserTimeZone;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.interbranch.InterBranchContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.enums.TransportInfoStatus;
import com.dpw.runner.shipment.services.commons.requests.AuditLogChanges;
import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.SortRequest;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.document.util.WorkbookMultipartFile;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.awb.AwbGoodsDescriptionInfo;
import com.dpw.runner.shipment.services.dto.request.intraBranch.InterBranchDto;
import com.dpw.runner.shipment.services.dto.request.mdm.MdmTaskCreateRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGRequestV3;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierRoutingResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonPackageResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.SailingInformationResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.SendEmailDto;
import com.dpw.runner.shipment.services.dto.v1.request.*;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.OceanDGStatus;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferAddress;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.notification.request.SendEmailBaseRequest;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.service.impl.TenantSettingsService;
import com.dpw.runner.shipment.services.service.interfaces.IApplicationConfigService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.itextpdf.text.*;
import com.itextpdf.text.pdf.*;
import lombok.extern.slf4j.Slf4j;
import net.sourceforge.barbecue.Barcode;
import net.sourceforge.barbecue.BarcodeException;
import net.sourceforge.barbecue.BarcodeFactory;
import net.sourceforge.barbecue.BarcodeImageHandler;
import net.sourceforge.barbecue.output.OutputException;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Workbook;
import org.jetbrains.annotations.Nullable;
import org.krysalis.barcode4j.impl.upcean.EAN13Bean;
import org.krysalis.barcode4j.output.bitmap.BitmapCanvasProvider;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.stereotype.Component;
import org.springframework.transaction.TransactionSystemException;
import org.springframework.util.CollectionUtils;
import org.springframework.web.multipart.MultipartFile;

import javax.imageio.ImageIO;
import javax.persistence.Entity;
import javax.persistence.EntityManager;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Selection;
import javax.validation.ConstraintViolation;
import javax.validation.ConstraintViolationException;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

@Component
@Slf4j
public class CommonUtils {
    private final INotificationService notificationService;

    @Autowired
    public CommonUtils(INotificationService notificationService) {
        this.notificationService = notificationService;
    }

    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    public ExecutorService syncExecutorService;

    @Autowired
    public IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private IAuditLogDao iAuditLogDao;

    @Autowired
    private TenantSettingsService tenantSettingsService;

    @Autowired
    private IV1Service iv1Service;

    @Autowired
    IShipmentDao shipmentDao;

    @Autowired
    IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    IQuoteContractsDao quoteContractsDao;

    @Autowired
    IMDMServiceAdapter mdmServiceAdapter;
    @Autowired
    private IV1Service v1Service;
    @Autowired
    private IApplicationConfigService applicationConfigService;

    @Autowired
    private EntityManager entityManager;

    @Autowired
    private ObjectMapper objectMapper;

    private static final Map<Class<?>, Type> DTO_TYPE_MAP = new HashMap<>();

    static {
        DTO_TYPE_MAP.put(Containers.class, new TypeToken<List<ContainerResponse>>() {}.getType());
        DTO_TYPE_MAP.put(BookingCarriage.class, new TypeToken<List<BookingCarriageResponse>>() {}.getType());
        DTO_TYPE_MAP.put(ELDetails.class, new TypeToken<List<ELDetailsResponse>>() {}.getType());
        DTO_TYPE_MAP.put(Events.class, new TypeToken<List<EventsResponse>>() {}.getType());
        DTO_TYPE_MAP.put(Packing.class, new TypeToken<List<PackingResponse>>() {}.getType());
        DTO_TYPE_MAP.put(ReferenceNumbers.class, new TypeToken<List<ReferenceNumbersResponse>>() {}.getType());
        DTO_TYPE_MAP.put(Routings.class, new TypeToken<List<RoutingsResponse>>() {}.getType());
        DTO_TYPE_MAP.put(ServiceDetails.class, new TypeToken<List<ServiceDetailsResponse>>() {}.getType());
        DTO_TYPE_MAP.put(TruckDriverDetails.class, new TypeToken<List<TruckDriverDetailsResponse>>() {}.getType());
        DTO_TYPE_MAP.put(Notes.class, new TypeToken<List<NotesResponse>>() {}.getType());
        DTO_TYPE_MAP.put(Jobs.class, new TypeToken<List<JobResponse>>() {}.getType());
        DTO_TYPE_MAP.put(ConsolidationDetails.class, new TypeToken<List<ConsolidationListResponse>>() {}.getType());
        DTO_TYPE_MAP.put(Parties.class, new TypeToken<List<PartiesResponse>>() {}.getType());
        DTO_TYPE_MAP.put(ShipmentOrder.class, new TypeToken<List<ShipmentOrderResponse>>() {}.getType());
        DTO_TYPE_MAP.put(CarrierRouting.class, new TypeToken<List<CarrierRoutingResponse>>() {}.getType());
        DTO_TYPE_MAP.put(CommonContainers.class, new TypeToken<List<CommonContainerResponse>>() {}.getType());
        DTO_TYPE_MAP.put(CommonPackages.class, new TypeToken<List<CommonPackageResponse>>() {}.getType());
    }

    private static final Map<String, ShipmentRequestedType> EMAIL_TYPE_MAPPING = new HashMap<>();

    static {
        EMAIL_TYPE_MAPPING.put(SHIPMENT_PULL_REQUESTED_EMAIL_TYPE, SHIPMENT_PULL_REQUESTED);
        EMAIL_TYPE_MAPPING.put(SHIPMENT_PULL_ACCEPTED_EMAIL_TYPE, SHIPMENT_PULL_ACCEPTED);
        EMAIL_TYPE_MAPPING.put(SHIPMENT_PULL_REJECTED_EMAIL_TYPE, SHIPMENT_PULL_REJECTED);
        EMAIL_TYPE_MAPPING.put(SHIPMENT_PUSH_REQUESTED_EMAIL_TYPE, SHIPMENT_PUSH_REQUESTED);
        EMAIL_TYPE_MAPPING.put(SHIPMENT_PUSH_ACCEPTED_EMAIL_TYPE, SHIPMENT_PUSH_ACCEPTED);
        EMAIL_TYPE_MAPPING.put(SHIPMENT_PUSH_REJECTED_EMAIL_TYPE, SHIPMENT_PUSH_REJECTED);
        EMAIL_TYPE_MAPPING.put(SHIPMENT_DETACH_EMAIL_TYPE, SHIPMENT_DETACH);
        EMAIL_TYPE_MAPPING.put(SHIPMENT_PULL_WITHDRAW_EMAIL_TYPE, SHIPMENT_PULL_WITHDRAW);
        EMAIL_TYPE_MAPPING.put(SHIPMENT_PUSH_WITHDRAW_EMAIL_TYPE, SHIPMENT_PUSH_WITHDRAW);
    }

    @Value("${current-base-url}")
    private String baseUrl;

    public static FilterCriteria constructCriteria(String fieldName, Object value, String operator, String logicalOperator) {
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        return FilterCriteria.builder().criteria(criteria).logicOperator(logicalOperator).build();
    }

    public static BufferedImage generateEAN13BarcodeImage(String barcodeText, int resolution) {
        EAN13Bean barcodeGenerator = new EAN13Bean();
        BitmapCanvasProvider canvas =
                new BitmapCanvasProvider(resolution, BufferedImage.TYPE_BYTE_BINARY, false, 0);

        barcodeGenerator.generateBarcode(canvas, barcodeText);
        return canvas.getBufferedImage();
    }

    public static byte[] generateBarcodeImage(String barcodeText) throws BarcodeException, OutputException {
        Barcode barcode = BarcodeFactory.createCode128(barcodeText);
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        BarcodeImageHandler.writePNG(barcode, outputStream);
        byte[] data = outputStream.toByteArray();
        outputStream.reset();
        return data;
    }

    public static ListCommonRequest constructListCommonRequest(String fieldName, Object value, String operator) {
        ListCommonRequest request = new ListCommonRequest();
        request.setPageNo(1);
        request.setPageSize(Integer.MAX_VALUE);


        List<FilterCriteria> criterias = new ArrayList<>();
        List<FilterCriteria> innerFilters = new ArrayList<>();
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        innerFilters.add(filterCriteria);
        criterias.add(FilterCriteria.builder().innerFilter(innerFilters).logicOperator(criterias.isEmpty() ? null : "or").build());
        request.setFilterCriteria(criterias);
        return request;
    }
    public static ListCommonRequest constructListRequestFromEntityId(Long entityId, String entityType) {
        FilterCriteria entityIdCriteria = FilterCriteria.builder()
                .innerFilter(Arrays.asList(FilterCriteria.builder()
                                .criteria(Criteria.builder()
                                        .fieldName("entityId")
                                        .operator("=")
                                        .value(entityId)
                                        .build()).build(),
                        FilterCriteria.builder()
                                .logicOperator("AND")
                                .criteria(Criteria.builder()
                                        .fieldName("entityType")
                                        .operator("=")
                                        .value(entityType)
                                        .build())
                                .build()))
                .build();

        return ListCommonRequest.builder()
                .pageNo(1)
                .pageSize(Integer.MAX_VALUE)
                .filterCriteria(Arrays.asList(entityIdCriteria))
                .build();
    }

    public static Criteria getFilterCriteria(String fieldName, Object value, String operator) {
        return Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
    }

    public static ListCommonRequest andCriteria(String fieldName, Object value, String operator, ListCommonRequest request) {
        if (request == null) {
            request = new ListCommonRequest();
            request.setPageNo(1);
            request.setPageSize(Integer.MAX_VALUE);
            request.setFilterCriteria(Arrays.asList(FilterCriteria.builder().innerFilter(new ArrayList<>()).build()));
        }

        if (request.getFilterCriteria() == null)
            request.setFilterCriteria(new ArrayList<>());

        List<FilterCriteria> criterias = request.getFilterCriteria();
        if (criterias.isEmpty()) {
            criterias.add(FilterCriteria.builder().innerFilter(new ArrayList<>()).build());
        }
        List<FilterCriteria> innerFilters = criterias.get(0).getInnerFilter();
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        if (!innerFilters.isEmpty()) {
            filterCriteria.setLogicOperator("and");
        }
        innerFilters.add(filterCriteria);
        return request;
    }

    public static ListCommonRequest orCriteria(String fieldName, Object value, String operator, ListCommonRequest request) {
        if (request == null) {
            request = new ListCommonRequest();
            request.setPageNo(1);
            request.setPageSize(Integer.MAX_VALUE);
            request.setFilterCriteria(Arrays.asList(FilterCriteria.builder().innerFilter(new ArrayList<>()).build()));
        }

        List<FilterCriteria> criterias = request.getFilterCriteria();
        List<FilterCriteria> innerFilters = criterias.get(0).getInnerFilter();
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        if (!innerFilters.isEmpty()) {
            filterCriteria.setLogicOperator("or");
        }
        innerFilters.add(filterCriteria);
        return request;
    }

    public <T, P> P convertToClass(T obj, Class<P> clazz) {
        return jsonHelper.convertValue(obj, clazz);
    }

    public <T, P extends IRunnerResponse> List<P> convertToDtoList(final List<T> lst, Class<P> clazz) {
        return lst.stream()
                .map(item -> convertToClass(item, clazz))
                .toList();
    }

    public <T, P extends MultiTenancy> List<P> convertToEntityList(final List<T> lst, Class<P> clazz) {
        return lst.stream()
                .map(item -> convertToClass(item, clazz))
                .toList();
    }

    public <T, P extends MultiTenancy> List<P> convertToEntityList(final List<T> lst, Class<P> clazz, Boolean isCreate) {
        return lst.stream()
                .map(item -> Boolean.TRUE.equals(isCreate) ? this.convertToCreateClass(item, clazz) : convertToClass(item, clazz))
                .toList();
    }

    public <T, P extends MultiTenancy> List<P> convertToCreateEntityList(final List<T> lst, Class<P> clazz) {
        return lst.stream()
                .map(item -> this.convertToCreateClass(item, clazz))
                .toList();
    }

    public <T, P> List<P> convertToList(final List<T> lst, Class<P> clazz) {
        if (lst == null)
            return null;
        return lst.stream()
                .map(item -> convertToClassModelMapper(item, clazz))
                .toList();
    }

    private <T, P> P convertToClassModelMapper(T obj, Class<P> clazz) {
        return modelMapper.map(obj, clazz);
    }

    public <T, P> P convertToCreateClass(T obj, Class<P> clazz) {
        return jsonHelper.convertCreateValue(obj, clazz);
    }

    public static byte[] imageToByte(BufferedImage img) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ImageIO.write(img, "jpg", baos);
        byte[] data = baos.toByteArray();
        baos.reset();
        return data;
    }

    public static boolean hasUnsupportedCharacters(String input) {
        int minSupportedAscii = 32;
        int maxSupportedAscii = 126;
        for (char c : input.toCharArray()) {
            if ( c < minSupportedAscii || c > maxSupportedAscii) {
                return true;
            }
        }
        return false;
    }

    public static byte[] concatAndAddContent(List<byte[]> pdfByteContent) throws DocumentException, IOException {
        ByteArrayOutputStream ms = new ByteArrayOutputStream();
        Document doc = new Document();
        PdfCopy copy =  new PdfCopy(doc, ms);
        doc.open();
        for (byte[] dataByte : pdfByteContent) {
            PdfReader reader = new PdfReader(dataByte);
            copy.addDocument(reader);
            reader.close();
        }
        doc.close();
        copy.close();
        byte[] data = ms.toByteArray();
        ms.reset();
        return data;
    }

    public static byte[] removeLastPage(byte[] bytes) throws IOException, DocumentException {
        PdfReader r = new PdfReader(bytes);
        ByteArrayOutputStream ms = new ByteArrayOutputStream();
        Document doc = new Document();
        PdfWriter w = PdfWriter.getInstance(doc, ms);
        doc.open();
        var pagesToKeep = r.getNumberOfPages();
        for (int page = 1; page < pagesToKeep; page++) {
            doc.newPage();
            w.getDirectContent().addTemplate(w.getImportedPage(r, page), 0, 0);
        }
        w.close();
        r.close();
        doc.close();
        byte[] data = ms.toByteArray();
        ms.reset();
        return data;
    }

    public static byte[] getLastPage(byte[] bytes) throws IOException, DocumentException {
        PdfReader r = new PdfReader(bytes);
        ByteArrayOutputStream ms = new ByteArrayOutputStream();
        Document doc = new Document();
        PdfWriter w = PdfWriter.getInstance(doc, ms);
        doc.open();
        doc.newPage();
        w.getDirectContent().addTemplate(w.getImportedPage(r, r.getNumberOfPages()), 0, 0);
        w.close();
        r.close();
        doc.close();
        byte[] data = ms.toByteArray();
        ms.reset();
        return data;
    }

    public static byte[] addBlankPage(byte[] originalPdfBytes) throws IOException, DocumentException {
        // Prepare a ByteArrayOutputStream to hold the modified PDF
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

        // Read the original PDF
        PdfReader reader = new PdfReader(originalPdfBytes);
        int numberOfPages = reader.getNumberOfPages();

        // Create a new Document for the output
        Document document = new Document();
        PdfWriter writer = PdfWriter.getInstance(document, outputStream);

        // Open the new document for writing
        document.open();

        // Copy all pages from the original PDF into the new document
        for (int i = 1; i <= numberOfPages; i++) {
            document.newPage();
            PdfImportedPage page = writer.getImportedPage(reader, i);
            writer.getDirectContent().addTemplate(page, 0, 0);
        }

        // Add a blank page
        document.newPage();
        writer.setPageEmpty(false);

        // Close the resources
        document.close();
        reader.close();

        // Return the modified PDF as a byte array
        return outputStream.toByteArray();
    }

    public static void addWaterMark(PdfContentByte dc, String text, BaseFont font, float fontSize, float angle, BaseColor color, Rectangle realPageSize, Rectangle rect) {
        var gstate = new PdfGState();
        gstate.setFillOpacity(0.2f);
        gstate.setStrokeOpacity(0.3f);
        dc.saveState();
        dc.setGState(gstate);
        dc.setColorFill(color);
        dc.beginText();
        dc.setFontAndSize(font, fontSize);
        var ps = rect == null ? realPageSize : rect; /*dc.PdfDocument.PageSize is not always correct*/
        var x = (ps.getRight() + ps.getLeft()) / 2;
        var y = (ps.getBottom() + ps.getTop()) / 2;
        dc.showTextAligned(Element.ALIGN_CENTER, text, x, y, angle);
        dc.endText();
        dc.restoreState();
    }

    public static byte[] addWatermarkToPdfBytes(byte[] bytes, BaseFont bf, String watermark) throws IOException, DocumentException {
        ByteArrayOutputStream ms = new ByteArrayOutputStream(10 * 1024);
        PdfReader reader = new PdfReader(bytes);
        PdfStamper stamper = new PdfStamper(reader, ms);
        int times = reader.getNumberOfPages();
        for (int i = 1; i <= times; i++) {
            var dc = stamper.getOverContent(i);
            addWaterMark(dc, watermark, bf, 50, 35, new BaseColor(70, 70, 255), reader.getPageSizeWithRotation(i), null);
        }
        stamper.close();
        reader.close();
        byte[] data = ms.toByteArray();
        ms.reset();
        return data;
    }

    public static ByteArrayResource getByteResource(InputStream inputStream, String fileName) throws IOException {
        return new ByteArrayResource(inputStream.readAllBytes()) {
            @Override
            public String getFilename() {
                return fileName;
            }
        };
    }

    public static double roundOffToTwoDecimalPlace(double number) {
        DecimalFormat decimalFormat = new DecimalFormat("#.##");
        return Double.parseDouble(decimalFormat.format(number));
    }

    public static String stringValueOf(Object o) {
        if (o == null)
            return null;
        return o.toString();
    }

    public static boolean isStringNullOrEmpty(String s) {
        return s == null || s.isEmpty();
    }

    public static <T> boolean listIsNullOrEmpty(List<T> list) {
        return list == null || list.isEmpty();
    }

    public static <T> boolean setIsNullOrEmpty(Set<T> set) {
        return set == null || set.isEmpty();
    }

    public static Integer getIntFromString(String s) {
        if (isStringNullOrEmpty(s))
            return null;
        return Integer.parseInt(s);
    }

    public static String getErrorResponseMessage(Exception e) {
        String responseMessage = "";
        responseMessage =
                switch (e.getClass().getSimpleName()) {
                    case "TransactionSystemException" -> Objects.requireNonNull(
                                    ((TransactionSystemException) e).getRootCause())
                            .getMessage();
                    default -> e.getMessage();
                };
        return responseMessage;
    }

    public static String getConstrainViolationErrorMessage(Exception e) {
        String errorMessage = "";
        Set<ConstraintViolation<?>> set = ((ConstraintViolationException) e).getConstraintViolations();
        List<String> errors = set.stream().map(i -> String.format("%s : %s", i.getInvalidValue(), i.getMessage())).toList();
        errorMessage = errors.toString();
        return errorMessage;
    }

    public static String inWords(Long num) {
        String[] a = {"", "One ", "Two ", "Three ", "Four ", "Five ", "Six ", "Seven ", "Eight ", "Nine ", "Ten ",
                "Eleven ", "Twelve ", "Thirteen ", "Fourteen ", "Fifteen ", "Sixteen ", "Seventeen ", "Eighteen ",
                "Nineteen "};
        String[] b = {"", "", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"};

        if (num > 999999999) {
            return "overflow";
        }

        String numStr = String.format("%09d", num);
        int[] n = {
                Integer.parseInt(numStr.substring(0, 2)), // Crore
                Integer.parseInt(numStr.substring(2, 4)), // Lakh
                Integer.parseInt(numStr.substring(4, 6)), // Thousand
                Integer.parseInt(numStr.substring(6, 7)), // Hundred
                Integer.parseInt(numStr.substring(7, 9))  // Tens and Ones
        };

        StringBuilder str = new StringBuilder();

        str.append((n[0] != 0) ? getTwoDigitWordConversion(a, b, n, 0) + "Crore " : "");
        str.append((n[1] != 0) ? getTwoDigitWordConversion(a, b, n, 1) + "Lakh " : "");
        str.append((n[2] != 0) ? getTwoDigitWordConversion(a, b, n, 2) + "Thousand " : "");
        String value = "";
        if (n[3] != 0) {
            value = !a[n[3]].isEmpty() ? a[n[3]] : b[n[3] / 10] + " " + a[n[3] % 10];
            value += " Hundred ";
        }
        str.append(value);

        value = "";
        if (n[4] != 0) {
            value = (!str.isEmpty() ? "and " : "") + getTwoDigitWordConversion(a, b, n, 4) + " ";
        }
        str.append(value);

        return str.toString().trim();
    }

    private static String getTwoDigitWordConversion(String[] a, String[] b, int[] n, int unitPlaceFromLeft) {
        if (a[n[unitPlaceFromLeft] % 10].equals(""))
            return b[n[unitPlaceFromLeft] / 10] + " ";
        else {
            if (n[unitPlaceFromLeft] / 10 != 0)
                return b[n[unitPlaceFromLeft] / 10] + " " + a[n[unitPlaceFromLeft] % 10];
            else
                return a[n[unitPlaceFromLeft] % 10];
        }
    }

    public static <T> Iterable<T> emptyIfNull(Iterable<T> iterable) {
        return iterable == null ? Collections.emptyList() : iterable;
    }

    public ShipmentSettingsDetails getShipmentSettingFromContext() {
        Optional<ShipmentSettingsDetails> optional = shipmentSettingsDao.getSettingsByTenantIdWithCache(TenantContext.getCurrentTenant());
        return optional.orElseGet(() -> ShipmentSettingsDetails.builder().weightDecimalPlace(2).volumeDecimalPlace(3).build());
    }


    public static boolean areTimeStampsEqual(LocalDateTime a, LocalDateTime b) {
        if (a == null || b == null)
            return false;
        var res = a.truncatedTo(ChronoUnit.MINUTES).compareTo(b.truncatedTo(ChronoUnit.MINUTES));
        return res == 0;
    }

    public V1TenantSettingsResponse getCurrentTenantSettings() {
        return tenantSettingsService.getV1TenantSettings(TenantContext.getCurrentTenant());
    }

    public InterBranchDto getInterBranchContext() {
        return InterBranchContext.getContext();
    }

    public void removeInterBranchContext() {
        InterBranchContext.removeContext();
    }

    public void setInterBranchContextForHub() {
        /**
         * Check current branch should be enabled both
         * Set isHub = true && coloadStationsTenantIds (TenantSettings + Current)
         */
        var tenantSettings = getCurrentTenantSettings();
        var interBranchDto = InterBranchDto.builder().build();

        if (Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())
                && Boolean.TRUE.equals(tenantSettings.getIsColoadingMAWBStationEnabled())
                && !Objects.isNull(tenantSettings.getColoadingBranchIds())) {
            interBranchDto.setColoadStationsTenantIds(tenantSettings.getColoadingBranchIds());
            interBranchDto.setHub(true);
        }

        InterBranchContext.setContext(interBranchDto);
    }

    public void setInterBranchContextForColoadStation() {
        /**
         * Check current branch should be enabled both IsMAWBColoadingEnabled
         * Set isCoLoadStation = true && hubTenantIds (TenantSettings + Current)
         */
        var tenantSettings = getCurrentTenantSettings();
        var interBranchDto = InterBranchDto.builder().hubTenantIds(Arrays.asList()).build();

        if (Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())
                && !Objects.isNull(tenantSettings.getColoadingBranchIds())) {
            interBranchDto.setHubTenantIds(fetchColoadingDetails().stream().map(CoLoadingMAWBDetailsResponse::getParentTenantId).toList());
            interBranchDto.setCoLoadStation(true);
        }

        InterBranchContext.setContext(interBranchDto);
    }

    public List<CoLoadingMAWBDetailsResponse> fetchColoadingDetails() {
        List<Object> criteria = new ArrayList<>(List.of(List.of("ChildTenantId"), "=", TenantContext.getCurrentTenant()));
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(100).criteriaRequests(criteria).build();
        var v1Response = iv1Service.getCoLoadingStations(commonV1ListRequest);
        return jsonHelper.convertValueToList(v1Response.entities, CoLoadingMAWBDetailsResponse.class);
    }

    public ConsolidationDetails calculateConsolUtilization(ConsolidationDetails consolidationDetails) throws RunnerException {
        String responseMsg;
        try {
            if (consolidationDetails.getAllocations() == null)
                consolidationDetails.setAllocations(new Allocations());
            if (consolidationDetails.getAchievedQuantities() == null)
                consolidationDetails.setAchievedQuantities(new AchievedQuantities());
            if (consolidationDetails.getAchievedQuantities().getConsolidatedWeightUnit() != null && consolidationDetails.getAllocations().getWeightUnit() != null) {
                BigDecimal consolidatedWeight = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAchievedQuantities().getConsolidatedWeight(), consolidationDetails.getAchievedQuantities().getConsolidatedWeightUnit(), Constants.WEIGHT_UNIT_KG).toString());
                BigDecimal weight = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAllocations().getWeight(), consolidationDetails.getAllocations().getWeightUnit(), Constants.WEIGHT_UNIT_KG).toString());
                if (Objects.equals(weight, BigDecimal.ZERO))
                    consolidationDetails.getAchievedQuantities().setWeightUtilization("0");
                else
                    consolidationDetails.getAchievedQuantities().setWeightUtilization(String.valueOf((consolidatedWeight.divide(weight, 4, RoundingMode.HALF_UP)).multiply(new BigDecimal(100)).doubleValue()));
            }
            if (consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit() != null && consolidationDetails.getAllocations().getVolumeUnit() != null) {
                BigDecimal consolidatedVolume = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAchievedQuantities().getConsolidatedVolume(), consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());
                BigDecimal volume = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAllocations().getVolume(), consolidationDetails.getAllocations().getVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());
                if (Objects.equals(volume, BigDecimal.ZERO))
                    consolidationDetails.getAchievedQuantities().setVolumeUtilization("0");
                else
                    consolidationDetails.getAchievedQuantities().setVolumeUtilization(String.valueOf((consolidatedVolume.divide(volume, 4, RoundingMode.HALF_UP)).multiply(new BigDecimal(100)).doubleValue()));
            }
            return consolidationDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    public void updateConsolOpenForAttachment(ConsolidationDetails consolidationDetails) {
        if (!Objects.isNull(consolidationDetails.getAchievedQuantities())) {
            Double weightUtilization = consolidationDetails.getAchievedQuantities().getWeightUtilization() != null ? Double.valueOf(consolidationDetails.getAchievedQuantities().getWeightUtilization()) : 0;
            Double volumeUtilization = consolidationDetails.getAchievedQuantities().getVolumeUtilization() != null ? Double.valueOf(consolidationDetails.getAchievedQuantities().getVolumeUtilization()) : 0;
            if (Objects.equals(consolidationDetails.getTransportMode(), TRANSPORT_MODE_AIR)
                    && (weightUtilization > 100 || volumeUtilization > 100))
                consolidationDetails.setOpenForAttachment(false);
        }
    }

    private void fetchDataForRejectionExplicitEmails(List<ShipmentDetails> shipmentDetails, List<ConsoleShipmentMapping> consoleShipmentMappings,
                                                     Set<Integer> tenantIds, Set<String> usernamesList, List<ConsolidationDetails> otherConsolidationDetails,
                                                     Map<String, String> usernameEmailsMap, Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests,
                                                     Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap) {
        for (ShipmentDetails shipmentDetails1 : shipmentDetails) {
            usernamesList.add(shipmentDetails1.getCreatedBy());
            usernamesList.add(shipmentDetails1.getAssignedTo());
            tenantIds.add(shipmentDetails1.getTenantId());
        }

        for (ConsoleShipmentMapping consoleShipmentMapping : consoleShipmentMappings) {
            usernamesList.add(consoleShipmentMapping.getCreatedBy());
        }
        for (ConsolidationDetails consolidationDetails1 : otherConsolidationDetails) {
            usernamesList.add(consolidationDetails1.getCreatedBy());
            if (consolidationDetails1.getAssignedTo() != null) {
                usernamesList.add(consolidationDetails1.getAssignedTo());
            }
            tenantIds.add(consolidationDetails1.getTenantId());
        }

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> getEmailTemplate(emailTemplatesRequests)), syncExecutorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)), syncExecutorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> getUserDetails(usernamesList, usernameEmailsMap)), syncExecutorService);
        CompletableFuture.allOf(emailTemplateFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();
    }

    public void sendRejectionEmailsExplicitly(List<ShipmentDetails> shipmentDetails, List<ConsoleShipmentMapping> consoleShipmentMappings,
                                              Set<ShipmentRequestedType> shipmentRequestedTypes, List<ConsolidationDetails> otherConsolidationDetails, boolean isV3FlagEnabled) {
        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();
        Map<String, String> usernameEmailsMap = new HashMap<>();
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests = new EnumMap<>(ShipmentRequestedType.class);
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> usernamesList = new HashSet<>();

        // fetch data from db and v1
        fetchDataForRejectionExplicitEmails(shipmentDetails, consoleShipmentMappings, tenantIds, usernamesList, otherConsolidationDetails, usernameEmailsMap, emailTemplatesRequests, v1TenantSettingsMap);

        if (!otherConsolidationDetails.isEmpty()) {
            Map<Long, ConsolidationDetails> finalConsolidationDetailsMap = otherConsolidationDetails.stream().collect(Collectors.toMap(BaseEntity::getId, y -> y));
            Map<Long, ShipmentDetails> finalShipmentDetailsMap = shipmentDetails.stream().collect(Collectors.toMap(BaseEntity::getId, e1 -> e1));
            consoleShipmentMappings.forEach(consoleShipmentMapping -> {
                try {
                    if (finalConsolidationDetailsMap.containsKey(consoleShipmentMapping.getConsolidationId()) && finalShipmentDetailsMap.containsKey(consoleShipmentMapping.getShipmentId())) {
                        if (consoleShipmentMapping.getRequestedType() == SHIPMENT_PUSH_REQUESTED)
                            sendEmailForPullPushRequestStatus(finalShipmentDetailsMap.get(consoleShipmentMapping.getShipmentId()), finalConsolidationDetailsMap.get(consoleShipmentMapping.getConsolidationId()), SHIPMENT_PUSH_REJECTED, AUTO_REJECTION_REMARK, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, consoleShipmentMapping.getCreatedBy(), null, isV3FlagEnabled);
                        else
                            sendEmailForPullPushRequestStatus(finalShipmentDetailsMap.get(consoleShipmentMapping.getShipmentId()), finalConsolidationDetailsMap.get(consoleShipmentMapping.getConsolidationId()), SHIPMENT_PULL_REJECTED, AUTO_REJECTION_REMARK, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, consoleShipmentMapping.getCreatedBy(), null, isV3FlagEnabled);
                    }
                } catch (Exception e) {
                    log.error(ERROR_WHILE_SENDING_EMAIL);
                }
            });
        }
    }

    public void sendEmailShipmentPullRequest(SendEmailDto sendEmailDto, boolean isV3FlagEnabled) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if (!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PULL_REQUESTED)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PULL_REQUESTED);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest = sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PULL_REQUESTED);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForPullRequested(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getUnLocMap(), sendEmailDto.getCarrierMasterDataMap());

        setShipmentCreateAndAssignedUserEmail(sendEmailDto, toEmailIds);
        setCurrentUserEmail(ccEmailIds);
        setConsolidationAssignedToUserEmail(sendEmailDto, ccEmailIds);

        if(isV3FlagEnabled) {
            setConsolidationCreatedUserEmail(sendEmailDto, ccEmailIds);
            setRequestedUserEmail(sendEmailDto, ccEmailIds);
            getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getShipmentDetails().getTenantId());
            if (Objects.nonNull(sendEmailDto.getConsolidationDetails()) && Objects.nonNull(sendEmailDto.getShipmentDetails().getTenantId())) {
                getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getConsolidationDetails().getTenantId());
            }
        } else {
            getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getShipmentDetails().getTenantId(), true);
        }
        // fetching to and cc from master lists

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailResponseToDGRequester(EmailTemplatesRequest template,
                                               OceanDGRequest request, ShipmentDetails shipmentDetails) {


        Map<String, Object> dictionary = new HashMap<>();
        List<String> recipientEmails = Collections.singletonList(request.getUserEmail());

        populateDGReceiverDictionary(dictionary, shipmentDetails, request);


        notificationService.sendEmail(replaceTagsFromData(dictionary, template.getBody()),
                template.getSubject(), new ArrayList<>(recipientEmails), new ArrayList<>());
    }

    public void sendEmailResponseToDGRequesterV3(EmailTemplatesRequest template,
        OceanDGRequestV3 request, ShipmentDetails shipmentDetails) {


        Map<String, Object> dictionary = new HashMap<>();
        List<String> recipientEmails = Collections.singletonList(request.getUserEmail());

        populateDGReceiverDictionaryV3(dictionary, shipmentDetails, request);


        notificationService.sendEmail(replaceTagsFromData(dictionary, template.getBody()),
            template.getSubject(), new ArrayList<>(recipientEmails), new ArrayList<>());
    }

    public void sendEmailShipmentPullAccept(SendEmailDto sendEmailDto, boolean isV3FlagEnabled) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if (!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PULL_ACCEPTED)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PULL_ACCEPTED);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest = sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PULL_ACCEPTED);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForPullAccepted(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getUnLocMap(), sendEmailDto.getCarrierMasterDataMap(), sendEmailDto.getRequestedUser());

        setConsolidationCreatedUserEmail(sendEmailDto, toEmailIds);
        setConsolidationAssignedToUserEmail(sendEmailDto, ccEmailIds);
        setRequestedUserEmail(sendEmailDto, ccEmailIds);
        setShipmentCreateAndAssignedUserEmail(sendEmailDto, ccEmailIds);
        setCurrentUserEmail(ccEmailIds);
        // fetching to and cc from master lists
        if (isV3FlagEnabled) {
            getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getConsolidationDetails().getTenantId());
            if(Objects.nonNull(sendEmailDto.getShipmentDetails()) && Objects.nonNull(sendEmailDto.getShipmentDetails().getTenantId())) {
                getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getShipmentDetails().getTenantId());
            }
        } else {
            getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getConsolidationDetails().getTenantId(), true);
        }

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void populateShipmentImportPullAttachmentTemplate(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, Map<String, CarrierMasterData> carrierMasterDataMap, Map<String, UnlocationsResponse> unLocMap) {
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String tsDateTimeFormat = v1TenantSettingsResponse.getDPWDateFormat();
        dictionary.put(SHIPMENT_CREATE_USER, shipmentDetails.getCreatedBy());
        dictionary.put(SHIPMENT_ASSIGNED_USER, shipmentDetails.getAssignedTo());
        if (!isStringNullOrEmpty(shipmentDetails.getAssignedTo()))
            dictionary.put(SHIPMENT_ASSIGNED_USER_WITH_SLASH, "/ " + shipmentDetails.getAssignedTo());
        dictionary.put(INTERBRANCH_SHIPMENT_NUMBER, getShipmentIdHyperLink(shipmentDetails.getShipmentId(), shipmentDetails.getId()));
        dictionary.put(INTERBRANCH_SHIPMENT_NUMBER_WITHOUT_LINK, shipmentDetails.getShipmentId());
        dictionary.put(CONSOL_BRANCH_CODE, UserContext.getUser().getCode());
        dictionary.put(CONSOL_BRANCH_NAME, UserContext.getUser().getTenantDisplayName());
        dictionary.put(SOURCE_CONSOLIDATION_NUMBER, consolidationDetails.getConsolidationNumber());
        dictionary.put(MAWB_NUMBER, consolidationDetails.getMawb());
        dictionary.put(ETD_CAPS, convertToDPWDateFormat(consolidationDetails.getCarrierDetails().getEtd(), tsDateTimeFormat));
        dictionary.put(ETA_CAPS, convertToDPWDateFormat(consolidationDetails.getCarrierDetails().getEta(), tsDateTimeFormat));
        if (!isStringNullOrEmpty(consolidationDetails.getCarrierDetails().getShippingLine()) && carrierMasterDataMap.containsKey(consolidationDetails.getCarrierDetails().getShippingLine())) {
            String carrierCode = carrierMasterDataMap.get(consolidationDetails.getCarrierDetails().getShippingLine()).getIataCode();
            if (isStringNullOrEmpty(carrierCode))
                carrierCode = carrierMasterDataMap.get(consolidationDetails.getCarrierDetails().getShippingLine()).getItemValue();
            dictionary.put(CARRIER_CODE, carrierCode);
            dictionary.put(CARRIER_NAME, carrierMasterDataMap.get(consolidationDetails.getCarrierDetails().getShippingLine()).getItemDescription());
        }
        dictionary.put(FLIGHT_NUMBER1, consolidationDetails.getCarrierDetails().getFlightNumber());
        if (!isStringNullOrEmpty(consolidationDetails.getCarrierDetails().getOriginPort()) && unLocMap.containsKey(consolidationDetails.getCarrierDetails().getOriginPort())) {
            dictionary.put(ReportConstants.POL, unLocMap.get(consolidationDetails.getCarrierDetails().getOriginPort()).getLocCode());
            dictionary.put(POL_NAME, unLocMap.get(consolidationDetails.getCarrierDetails().getOriginPort()).getName());
        }
        if (!isStringNullOrEmpty(consolidationDetails.getCarrierDetails().getDestinationPort()) && unLocMap.containsKey(consolidationDetails.getCarrierDetails().getDestinationPort())) {
            dictionary.put(ReportConstants.POD, unLocMap.get(consolidationDetails.getCarrierDetails().getDestinationPort()).getLocCode());
            dictionary.put(POD_NAME, unLocMap.get(consolidationDetails.getCarrierDetails().getDestinationPort()).getName());
        }
        dictionary.put(BRANCH_TIME_ZONE, MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME));
        dictionary.put(USER_NAME, consolidationDetails.getCreatedBy());
        dictionary.put(ACTIONED_USER_NAME, UserContext.getUser().getUsername());
        dictionary.put(REQUEST_DATE_TIME, convertToDPWDateFormat(convertDateToUserTimeZone(LocalDateTime.now(), MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME), null, false), tsDateTimeFormat));
    }

    public void populateShipmentImportPushAttachmentTemplate(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, Map<String, CarrierMasterData> carrierMasterDataMap, Map<String, UnlocationsResponse> unLocMap) {
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String tsDateTimeFormat = v1TenantSettingsResponse.getDPWDateFormat();
        dictionary.put(CONSOLIDATION_CREATE_USER, consolidationDetails.getCreatedBy());
        dictionary.put(SHIPMENT_NUMBER, shipmentDetails.getShipmentId());
        dictionary.put(CONSOL_BRANCH_CODE, UserContext.getUser().getCode());
        dictionary.put(CONSOL_BRANCH_NAME, UserContext.getUser().getTenantDisplayName());
        dictionary.put(SOURCE_CONSOLIDATION_NUMBER, consolidationDetails.getConsolidationNumber());
        dictionary.put(HAWB_NUMBER, shipmentDetails.getHouseBill());
        dictionary.put(ETD_CAPS, convertToDPWDateFormat(shipmentDetails.getCarrierDetails().getEtd(), tsDateTimeFormat));
        dictionary.put(ETA_CAPS, convertToDPWDateFormat(shipmentDetails.getCarrierDetails().getEta(), tsDateTimeFormat));
        if (!isStringNullOrEmpty(shipmentDetails.getCarrierDetails().getShippingLine()) && carrierMasterDataMap.containsKey(shipmentDetails.getCarrierDetails().getShippingLine())) {
            String carrierCode = carrierMasterDataMap.get(shipmentDetails.getCarrierDetails().getShippingLine()).getIataCode();
            if (isStringNullOrEmpty(carrierCode))
                carrierCode = carrierMasterDataMap.get(shipmentDetails.getCarrierDetails().getShippingLine()).getItemValue();
            dictionary.put(CARRIER_CODE, carrierCode);
            dictionary.put(CARRIER_NAME, carrierMasterDataMap.get(shipmentDetails.getCarrierDetails().getShippingLine()).getItemDescription());
        }
        dictionary.put(FLIGHT_NUMBER1, shipmentDetails.getCarrierDetails().getFlightNumber());
        if (!isStringNullOrEmpty(shipmentDetails.getCarrierDetails().getOriginPort()) && unLocMap.containsKey(shipmentDetails.getCarrierDetails().getOriginPort())) {
            dictionary.put(ReportConstants.POL, unLocMap.get(shipmentDetails.getCarrierDetails().getOriginPort()).getLocCode());
            dictionary.put(POL_NAME, unLocMap.get(shipmentDetails.getCarrierDetails().getOriginPort()).getName());
        }
        if (!isStringNullOrEmpty(shipmentDetails.getCarrierDetails().getDestinationPort()) && unLocMap.containsKey(shipmentDetails.getCarrierDetails().getDestinationPort())) {
            dictionary.put(ReportConstants.POD, unLocMap.get(shipmentDetails.getCarrierDetails().getDestinationPort()).getLocCode());
            dictionary.put(POD_NAME, unLocMap.get(shipmentDetails.getCarrierDetails().getDestinationPort()).getName());
        }
        dictionary.put(BRANCH_TIME_ZONE, MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME));
        dictionary.put(REQUEST_DATE_TIME, convertToDPWDateFormat(convertDateToUserTimeZone(LocalDateTime.now(), MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME), null, false), tsDateTimeFormat));
        dictionary.put(USER_NAME, shipmentDetails.getCreatedBy());
        dictionary.put(ACTIONED_USER_NAME, UserContext.getUser().getUsername());
    }

    public void sendEmailNotification(Map<String, Object> dictionary, EmailTemplatesRequest emailTemplateModel, List<String> to, List<String> cc) {
        if (!to.isEmpty()) {
            try {
                notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplateModel.getBody()),
                        replaceTagsFromData(dictionary, emailTemplateModel.getSubject()), to, cc);
            } catch (Exception ex) {
                log.error(ex.getMessage());
            }
        }
    }

    public List<EmailTemplatesRequest> getEmailTemplates(String templateType) {
        List<String> requests = new ArrayList<>(List.of(templateType));
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> field = new ArrayList<>(List.of(Constants.TYPE));
        String operator = Operators.IN.getValue();
        List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
        request.setCriteriaRequests(criteria);
        V1DataResponse v1DataResponse = iv1Service.getEmailTemplates(request);
        return jsonHelper.convertValueToList(v1DataResponse.entities, EmailTemplatesRequest.class);
    }

    public void sendEmailShipmentPullReject(SendEmailDto sendEmailDto, boolean isV3FlagEnabled) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if (!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PULL_REJECTED)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PULL_REJECTED);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest = sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PULL_REJECTED);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForPullRejected(dictionary, sendEmailDto.getConsolidationDetails(), sendEmailDto.getRejectRemarks(), sendEmailDto.getRequestedUser());

        setConsolidationCreatedUserEmail(sendEmailDto, toEmailIds);
        setConsolidationAssignedToUserEmail(sendEmailDto, ccEmailIds);
        setRequestedUserEmail(sendEmailDto, ccEmailIds);
        setShipmentCreateAndAssignedUserEmail(sendEmailDto, ccEmailIds);
        setCurrentUserEmail(ccEmailIds);
        if (isV3FlagEnabled) {
            // fetching to and cc from master lists
            getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getConsolidationDetails().getTenantId());
            if(Objects.nonNull(sendEmailDto.getShipmentDetails()) && Objects.nonNull(sendEmailDto.getShipmentDetails().getTenantId())) {
                getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getShipmentDetails().getTenantId());
            }
        } else {
            getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getConsolidationDetails().getTenantId(), true);
        }

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailShipmentPushRequest(SendEmailDto sendEmailDto, boolean isV3FlagEnabled) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if (!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PUSH_REQUESTED)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PUSH_REQUESTED);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest = sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PUSH_REQUESTED);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForPushRequested(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getUnLocMap(), sendEmailDto.getCarrierMasterDataMap());

        setConsolidationCreatedUserEmail(sendEmailDto, toEmailIds);
        setConsolidationAssignedToUserEmail(sendEmailDto, ccEmailIds);
        setShipmentCreateAndAssignedUserEmail(sendEmailDto, ccEmailIds);
        setCurrentUserEmail(ccEmailIds);
        // fetching to and cc from master lists
        if(isV3FlagEnabled) {
            getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getConsolidationDetails().getTenantId());
            if(Objects.nonNull(sendEmailDto.getShipmentDetails()) && Objects.nonNull(sendEmailDto.getShipmentDetails().getTenantId())) {
                getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getShipmentDetails().getTenantId());
            }
        } else {
            getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getConsolidationDetails().getTenantId(), false);
        }
        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailShipmentPushAccept(SendEmailDto sendEmailDto, boolean isV3FlagEnabled) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if (!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PUSH_ACCEPTED)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PUSH_ACCEPTED);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest = sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PUSH_ACCEPTED);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForPushAccepted(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getUnLocMap(), sendEmailDto.getCarrierMasterDataMap(), sendEmailDto.getRequestedUser());

        setShipmentCreateAndAssignedUserEmail(sendEmailDto, toEmailIds);
        setRequestedUserEmail(sendEmailDto, ccEmailIds);
        setCurrentUserEmail(ccEmailIds);
        setConsolidationAssignedToUserEmail(sendEmailDto, ccEmailIds);
        if(isV3FlagEnabled) {
            setConsolidationCreatedUserEmail(sendEmailDto, ccEmailIds);
            getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getShipmentDetails().getTenantId());
            if (Objects.nonNull(sendEmailDto.getConsolidationDetails()) && Objects.nonNull(sendEmailDto.getConsolidationDetails().getTenantId())) {
                getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getConsolidationDetails().getTenantId());
            }
        } else {
            getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getShipmentDetails().getTenantId(), false);
        }
        // fetching to and cc from master lists

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailShipmentPushReject(SendEmailDto sendEmailDto, boolean isV3FlagEnabled) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if (!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PUSH_REJECTED)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PUSH_REJECTED);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest = sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PUSH_REJECTED);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForPushRejected(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getRejectRemarks(), sendEmailDto.getRequestedUser());

        setShipmentCreateAndAssignedUserEmail(sendEmailDto, toEmailIds);
        setRequestedUserEmail(sendEmailDto, ccEmailIds);
        setConsolidationAssignedToUserEmail(sendEmailDto, ccEmailIds);
        setCurrentUserEmail(ccEmailIds);
        if (isV3FlagEnabled) {
            setConsolidationCreatedUserEmail(sendEmailDto, ccEmailIds);
            getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getShipmentDetails().getTenantId());
            if(Objects.nonNull(sendEmailDto.getConsolidationDetails()) && Objects.nonNull(sendEmailDto.getConsolidationDetails().getTenantId())) {
                getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getConsolidationDetails().getTenantId());
            }
        } else {
            getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getShipmentDetails().getTenantId(), false);
        }
        // fetching to and cc from master lists

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailShipmentDetach(SendEmailDto sendEmailDto, boolean isV3FlagEnabled) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if (!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_DETACH)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_DETACH);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest = sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_DETACH);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForShipmentDetach(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getRejectRemarks());

        setShipmentCreateAndAssignedUserEmail(sendEmailDto, toEmailIds);
        setConsolidationCreatedUserEmail(sendEmailDto, ccEmailIds);
        setConsolidationAssignedToUserEmail(sendEmailDto, ccEmailIds);
        setCurrentUserEmail(ccEmailIds);
        // fetching to and cc from master lists
        if (isV3FlagEnabled) {
            getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getShipmentDetails().getTenantId());
            if (Objects.nonNull(sendEmailDto.getConsolidationDetails()) && Objects.nonNull(sendEmailDto.getConsolidationDetails().getTenantId())) {
                getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getConsolidationDetails().getTenantId());
            }
        } else {
            getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getShipmentDetails().getTenantId(), true);
        }

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailShipmentPullWithdraw(SendEmailDto sendEmailDto) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if (!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PULL_WITHDRAW)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PULL_WITHDRAW);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest = sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PULL_WITHDRAW);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForShipmentWithdraw(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getRejectRemarks(), sendEmailDto.getTenantModelMap());

        setShipmentCreateAndAssignedUserEmail(sendEmailDto, toEmailIds);
        setConsolidationCreatedUserEmail(sendEmailDto, ccEmailIds);
        setConsolidationAssignedToUserEmail(sendEmailDto, ccEmailIds);
        setCurrentUserEmail(ccEmailIds);
        // fetching to and cc from master lists
        if (sendEmailDto.getV1TenantSettingsMap().containsKey(sendEmailDto.getShipmentDetails().getTenantId())) {
            if (!isStringNullOrEmpty(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getShipmentDetails().getTenantId()).getShipmentAttachDefaultToMailId()))
                toEmailIds.addAll(Arrays.stream(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getShipmentDetails().getTenantId()).getShipmentAttachDefaultToMailId().split(",")).map(String::trim)
                        .filter(s -> !s.isEmpty()).toList());
            if (!isStringNullOrEmpty(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getShipmentDetails().getTenantId()).getConsolidationAttachDefaultToMailId()))
                toEmailIds.addAll(Arrays.stream(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getShipmentDetails().getTenantId()).getConsolidationAttachDefaultToMailId().split(",")).map(String::trim)
                        .filter(s -> !s.isEmpty()).toList());
        }
        if (sendEmailDto.getV1TenantSettingsMap().containsKey(sendEmailDto.getConsolidationDetails().getTenantId()) &&
                !isStringNullOrEmpty(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getConsolidationDetails().getTenantId()).getConsolidationAttachDefaultCCMailId())) {
            ccEmailIds.addAll(Arrays.stream(sendEmailDto.getV1TenantSettingsMap()
                            .get(sendEmailDto.getConsolidationDetails().getTenantId())
                            .getConsolidationAttachDefaultCCMailId()
                            .split(","))
                    .map(String::trim)
                    .filter(s -> !s.isEmpty())
                    .toList());
        }

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailShipmentPushWithdraw(SendEmailDto sendEmailDto) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if (!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PUSH_WITHDRAW)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PUSH_WITHDRAW);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest = sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PUSH_WITHDRAW);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForConsolidationWithdraw(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getRejectRemarks(), sendEmailDto.getTenantModelMap());

        setShipmentCreateAndAssignedUserEmail(sendEmailDto, ccEmailIds);
        setConsolidationCreatedUserEmail(sendEmailDto, toEmailIds);
        setConsolidationAssignedToUserEmail(sendEmailDto, ccEmailIds);
        setRequestedUserEmail(sendEmailDto, toEmailIds);
        setCurrentUserEmail(ccEmailIds);
        // fetching to and cc from master lists
        if (sendEmailDto.getV1TenantSettingsMap().containsKey(sendEmailDto.getConsolidationDetails().getTenantId())) {
            if (!isStringNullOrEmpty(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getConsolidationDetails().getTenantId()).getConsolidationAttachDefaultToMailId()))
                toEmailIds.addAll(Arrays.stream(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getConsolidationDetails().getTenantId()).getConsolidationAttachDefaultToMailId().split(",")).map(String::trim)
                        .filter(s -> !s.isEmpty()).toList());
            if (!isStringNullOrEmpty(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getConsolidationDetails().getTenantId()).getShipmentAttachDefaultToMailId()))
                toEmailIds.addAll(Arrays.stream(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getConsolidationDetails().getTenantId()).getShipmentAttachDefaultToMailId().split(",")).map(String::trim)
                        .filter(s -> !s.isEmpty()).toList());
        }
        if (sendEmailDto.getV1TenantSettingsMap().containsKey(sendEmailDto.getShipmentDetails().getTenantId())) {
            if (!isStringNullOrEmpty(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getShipmentDetails().getTenantId()).getConsolidationAttachDefaultCCMailId())) {
                ccEmailIds.addAll(Arrays.stream(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getShipmentDetails().getTenantId()).getConsolidationAttachDefaultCCMailId().split(",")).map(String::trim)
                        .filter(s -> !s.isEmpty()).toList());
            }
            if (!isStringNullOrEmpty(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getShipmentDetails().getTenantId()).getShipmentAttachDefaultCCMailId())) {
                ccEmailIds.addAll(Arrays.stream(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getShipmentDetails().getTenantId()).getShipmentAttachDefaultCCMailId().split(",")).map(String::trim)
                        .filter(s -> !s.isEmpty()).toList());
            }
        }

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailForPullPushRequestStatus(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, ShipmentRequestedType type, String rejectRemarks,
                                                  Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequestMap, Set<ShipmentRequestedType> shipmentRequestedTypes, Map<String, UnlocationsResponse> unLocMap,
                                                  Map<String, CarrierMasterData> carrierMasterDataMap, Map<String, String> usernameEmailsMap, Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap,
                                                  String requestedUser, Map<Integer, TenantModel> tenantModelMap, boolean isV3FlagEnabled) throws Exception {
        SendEmailDto sendEmailDto = SendEmailDto.builder()
                .shipmentDetails(shipmentDetails)
                .consolidationDetails(consolidationDetails)
                .type(type)
                .rejectRemarks(rejectRemarks)
                .emailTemplatesRequestMap(emailTemplatesRequestMap)
                .shipmentRequestedTypes(shipmentRequestedTypes)
                .unLocMap(unLocMap)
                .carrierMasterDataMap(carrierMasterDataMap)
                .usernameEmailsMap(usernameEmailsMap)
                .v1TenantSettingsMap(v1TenantSettingsMap)
                .requestedUser(requestedUser)
                .tenantModelMap(tenantModelMap)
                .build();
        sendEmailForPullPushRequestStatus(sendEmailDto, isV3FlagEnabled);
    }

    public void sendEmailForPullPushRequestStatus(SendEmailDto sendEmailDto, boolean isV3FlagEnabled) throws Exception {
        switch (sendEmailDto.getType()) {
            case SHIPMENT_PULL_REQUESTED -> sendEmailShipmentPullRequest(sendEmailDto, isV3FlagEnabled);
            case SHIPMENT_PULL_ACCEPTED -> sendEmailShipmentPullAccept(sendEmailDto, isV3FlagEnabled);
            case SHIPMENT_PULL_REJECTED -> sendEmailShipmentPullReject(sendEmailDto, isV3FlagEnabled);
            case SHIPMENT_PUSH_REQUESTED -> sendEmailShipmentPushRequest(sendEmailDto, isV3FlagEnabled);
            case SHIPMENT_PUSH_ACCEPTED -> sendEmailShipmentPushAccept(sendEmailDto, isV3FlagEnabled);
            case SHIPMENT_PUSH_REJECTED -> sendEmailShipmentPushReject(sendEmailDto, isV3FlagEnabled);
            case SHIPMENT_DETACH -> sendEmailShipmentDetach(sendEmailDto, isV3FlagEnabled);
            case SHIPMENT_PULL_WITHDRAW -> sendEmailShipmentPullWithdraw(sendEmailDto);
            case SHIPMENT_PUSH_WITHDRAW -> sendEmailShipmentPushWithdraw(sendEmailDto);
            default -> log.debug(Constants.SWITCH_DEFAULT_CASE_MSG, sendEmailDto.getType(), isV3FlagEnabled);
        }
    }

    public void setShipmentCreateAndAssignedUserEmail(SendEmailDto sendEmailDto, Set<String> emailIds) {
        if (!isStringNullOrEmpty(sendEmailDto.getShipmentDetails().getAssignedTo()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getShipmentDetails().getAssignedTo()))
            emailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getShipmentDetails().getAssignedTo()));
        if (!isStringNullOrEmpty(sendEmailDto.getShipmentDetails().getCreatedBy()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getShipmentDetails().getCreatedBy()))
            emailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getShipmentDetails().getCreatedBy()));
    }

    public static void setConsolidationCreatedUserEmail(SendEmailDto sendEmailDto, Set<String> emailIds) {
        if (!isStringNullOrEmpty(sendEmailDto.getConsolidationDetails().getCreatedBy()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getConsolidationDetails().getCreatedBy()))
            emailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getConsolidationDetails().getCreatedBy()));
    }
    private void setConsolidationAssignedToUserEmail(SendEmailDto sendEmailDto, Set<String> emailIds) {
        if (!isStringNullOrEmpty(sendEmailDto.getConsolidationDetails().getAssignedTo()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getConsolidationDetails().getAssignedTo())) {
            emailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getConsolidationDetails().getAssignedTo()));
        }
    }

    public void setRequestedUserEmail(SendEmailDto sendEmailDto, Set<String> emailIds) {
        if (!isStringNullOrEmpty(sendEmailDto.getRequestedUser()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getRequestedUser()))
            emailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getRequestedUser()));
    }

    public void setCurrentUserEmail(Set<String> emailIds) {
        if (!isStringNullOrEmpty(UserContext.getUser().getEmail()))
            emailIds.add(UserContext.getUser().getEmail());
    }

    public void getToAndCcEmailMasterLists(Set<String> toEmailIds, Set<String> ccEmailIds, Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap, Integer tenantId) {
        if (v1TenantSettingsMap.containsKey(tenantId)) {
            V1TenantSettingsResponse settings = v1TenantSettingsMap.get(tenantId);
            addEmails(toEmailIds, settings.getShipmentAttachDefaultToMailId());
            addEmails(toEmailIds, settings.getConsolidationAttachDefaultToMailId());
            addEmails(ccEmailIds, settings.getShipmentAttachDefaultCCMailId());
            addEmails(ccEmailIds, settings.getConsolidationAttachDefaultCCMailId());
        }
    }

    public void getToAndCcEmailMasterLists(Set<String> toEmailIds, Set<String> ccEmailIds, Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap, Integer tenantId, boolean isShipment) {
        if (v1TenantSettingsMap.containsKey(tenantId)) {
            V1TenantSettingsResponse settings = v1TenantSettingsMap.get(tenantId);
            if (isShipment) {
                addEmails(toEmailIds, settings.getShipmentAttachDefaultToMailId());
                addEmails(ccEmailIds, settings.getShipmentAttachDefaultCCMailId());
            } else {
                addEmails(toEmailIds, settings.getConsolidationAttachDefaultToMailId());
                addEmails(ccEmailIds, settings.getConsolidationAttachDefaultCCMailId());
            }
        }
    }

    private void addEmails(Set<String> emailIds, String emailString) {
        if (!isStringNullOrEmpty(emailString)) {
            List<String> emails = Arrays.stream(emailString.split(","))
                    .map(String::trim)
                    .filter(s -> !s.isEmpty())
                    .collect(Collectors.toList());
            emailIds.addAll(emails);
        }
    }

    public void populateDictionaryForPullRequested(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                                   Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap) {
        populateDictionaryForEmailFromConsolidation(dictionary, shipmentDetails, consolidationDetails, unLocMap, carrierMasterDataMap);
        dictionary.put(CONSOL_BRANCH_CODE, UserContext.getUser().getCode());
        dictionary.put(CONSOL_BRANCH_NAME, UserContext.getUser().getTenantDisplayName());
        dictionary.put(USER_NAME, consolidationDetails.getCreatedBy());
        dictionary.put(REQUESTED_USER_NAME, UserContext.getUser().getUsername());
    }

    public void populateDictionaryForPullAccepted(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                                  Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap, String requestedUser) {
        populateDictionaryForEmailFromShipment(dictionary, shipmentDetails, consolidationDetails, unLocMap, carrierMasterDataMap);
        dictionary.put(ACTIONED_USER_NAME, UserContext.getUser().getUsername());
        dictionary.put(REQUESTED_USER_NAME, requestedUser);
    }

    public void populateDictionaryForPullRejected(Map<String, Object> dictionary, ConsolidationDetails consolidationDetails, String rejectRemarks, String requestedUser) {
        dictionary.put(CONSOLIDATION_CREATE_USER, consolidationDetails.getCreatedBy());
        dictionary.put(SHIPMENT_BRANCH_CODE, UserContext.getUser().getCode());
        dictionary.put(SHIPMENT_BRANCH_NAME, UserContext.getUser().getTenantDisplayName());
        dictionary.put(INTERBRANCH_CONSOLIDATION_NUMBER, getConsolidationIdHyperLink(consolidationDetails.getConsolidationNumber(), consolidationDetails.getId()));
        dictionary.put(INTERBRANCH_CONSOLIDATION_NUMBER_WITHOUT_LINK, consolidationDetails.getConsolidationNumber());
        dictionary.put(Constants.REJECT_REMARKS, rejectRemarks);
        dictionary.put(ACTIONED_USER_NAME, UserContext.getUser().getUsername());
        dictionary.put(REQUESTED_USER_NAME, requestedUser);
    }

    public void populateDictionaryForPushRequested(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                                   Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap) {
        populateDictionaryForEmailFromShipment(dictionary, shipmentDetails, consolidationDetails, unLocMap, carrierMasterDataMap);
        dictionary.put(USER_NAME, shipmentDetails.getCreatedBy());
        dictionary.put(REQUESTED_USER_NAME, UserContext.getUser().getUsername());
    }

    public void populateDictionaryForPushAccepted(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                                  Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap, String requestedUser) {
        populateDictionaryForEmailFromConsolidation(dictionary, shipmentDetails, consolidationDetails, unLocMap, carrierMasterDataMap);
        dictionary.put(ACTIONED_USER_NAME, UserContext.getUser().getUsername());
        dictionary.put(REQUESTED_USER_NAME, requestedUser);
    }

    public void populateDictionaryForPushRejected(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, String rejectRemarks, String requestUser) {
        dictionary.put(SHIPMENT_CREATE_USER, shipmentDetails.getCreatedBy());
        dictionary.put(SHIPMENT_ASSIGNED_USER, shipmentDetails.getAssignedTo());
        if (!isStringNullOrEmpty(shipmentDetails.getAssignedTo()))
            dictionary.put(SHIPMENT_ASSIGNED_USER_WITH_SLASH, "/ " + shipmentDetails.getAssignedTo());
        dictionary.put(INTERBRANCH_SHIPMENT_NUMBER, getShipmentIdHyperLink(shipmentDetails.getShipmentId(), shipmentDetails.getId()));
        dictionary.put(INTERBRANCH_SHIPMENT_NUMBER_WITHOUT_LINK, shipmentDetails.getShipmentId());
        dictionary.put(SOURCE_CONSOLIDATION_NUMBER, consolidationDetails.getConsolidationNumber());
        dictionary.put(Constants.REJECT_REMARKS, rejectRemarks);
        dictionary.put(ACTIONED_USER_NAME, UserContext.getUser().getUsername());
        dictionary.put(REQUESTED_USER_NAME, requestUser);
    }

    public void populateDictionaryForShipmentWithdraw(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, String remarks,
                                                      Map<Integer, TenantModel> tenantModelMap) {
        dictionary.put(SHIPMENT_CREATE_USER, shipmentDetails.getCreatedBy());
        dictionary.put(SHIPMENT_ASSIGNED_USER, shipmentDetails.getAssignedTo());
        if (!isStringNullOrEmpty(shipmentDetails.getAssignedTo()))
            dictionary.put(SHIPMENT_ASSIGNED_USER_WITH_SLASH, "/ " + shipmentDetails.getAssignedTo());
        dictionary.put(REGIONAL_BRANCH_CODE, tenantModelMap.get(shipmentDetails.getTenantId()).getCode());
        dictionary.put(REGIONAL_BRANCH_NAME, tenantModelMap.get(shipmentDetails.getTenantId()).getTenantName());
        dictionary.put(INTERBRANCH_SHIPMENT_NUMBER, getShipmentIdHyperLink(shipmentDetails.getShipmentId(), shipmentDetails.getId()));
        dictionary.put(INTERBRANCH_SHIPMENT_NUMBER_WITHOUT_LINK, shipmentDetails.getShipmentId());
        dictionary.put(Constants.CONSOLIDATION_NUMBER, consolidationDetails.getConsolidationNumber());
        dictionary.put(SOURCE_CONSOLIDATION_NUMBER, consolidationDetails.getConsolidationNumber());
        dictionary.put(Constants.WITHDRAW_REMARKS, remarks);
        dictionary.put(USER_NAME, UserContext.getUser().getUsername());
    }

    public void populateDictionaryForConsolidationWithdraw(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, String remarks,
                                                           Map<Integer, TenantModel> tenantModelMap) {
        dictionary.put(CONSOLIDATION_CREATE_USER, consolidationDetails.getCreatedBy());
        dictionary.put(HUB_BRANCH_CODE, tenantModelMap.get(consolidationDetails.getTenantId()).getCode());
        dictionary.put(HUB_BRANCH_NAME, tenantModelMap.get(consolidationDetails.getTenantId()).getTenantName());
        dictionary.put(INTERBRANCH_CONSOLIDATION_NUMBER, getConsolidationIdHyperLink(consolidationDetails.getConsolidationNumber(), consolidationDetails.getId()));
        dictionary.put(INTERBRANCH_CONSOLIDATION_NUMBER_WITHOUT_LINK, consolidationDetails.getConsolidationNumber());
        dictionary.put(SHIPMENT_NUMBER, shipmentDetails.getShipmentId());
        dictionary.put(SOURCE_CONSOLIDATION_NUMBER, consolidationDetails.getConsolidationNumber());
        dictionary.put(Constants.WITHDRAW_REMARKS, remarks);
        dictionary.put(ACTIONED_USER_NAME, UserContext.getUser().getUsername());
        dictionary.put(SHIPMENT_CREATE_USER, shipmentDetails.getCreatedBy());
        dictionary.put(REQUESTED_USER_NAME, UserContext.getUser().getUsername());
    }

    public void populateDictionaryForShipmentDetach(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, String detachRemarks) {
        dictionary.put(SHIPMENT_CREATE_USER, shipmentDetails.getCreatedBy());
        dictionary.put(SHIPMENT_ASSIGNED_USER, shipmentDetails.getAssignedTo());
        if (!isStringNullOrEmpty(shipmentDetails.getAssignedTo()))
            dictionary.put(SHIPMENT_ASSIGNED_USER_WITH_SLASH, "/ " + shipmentDetails.getAssignedTo());
        dictionary.put(INTERBRANCH_CONSOLIDATION_NUMBER, getConsolidationIdHyperLink(consolidationDetails.getConsolidationNumber(), consolidationDetails.getId()));
        dictionary.put(INTERBRANCH_CONSOLIDATION_NUMBER_WITHOUT_LINK, consolidationDetails.getConsolidationNumber());
        dictionary.put(SHIPMENT_NUMBER, shipmentDetails.getShipmentId());
        dictionary.put(Constants.REJECT_REMARKS, detachRemarks);
        dictionary.put(ACTIONED_USER_NAME, UserContext.getUser().getUsername());
        dictionary.put(REQUESTED_USER_NAME, UserContext.getUser().getUsername());
    }

    public void populateDictionaryForEmailFromShipment(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                                       Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap) {
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String tsDateTimeFormat = v1TenantSettingsResponse.getDPWDateFormat();
        dictionary.put(CONSOLIDATION_CREATE_USER, consolidationDetails.getCreatedBy());
        dictionary.put(SHIPMENT_BRANCH_CODE, UserContext.getUser().getCode());
        dictionary.put(SHIPMENT_BRANCH_NAME, UserContext.getUser().getTenantDisplayName());
        dictionary.put(INTERBRANCH_CONSOLIDATION_NUMBER, getConsolidationIdHyperLink(consolidationDetails.getConsolidationNumber(), consolidationDetails.getId()));
        dictionary.put(INTERBRANCH_CONSOLIDATION_NUMBER_WITHOUT_LINK, consolidationDetails.getConsolidationNumber());
        dictionary.put(SHIPMENT_NUMBER, shipmentDetails.getShipmentId());
        dictionary.put(HAWB_NUMBER, shipmentDetails.getHouseBill());
        dictionary.put(ETD_CAPS, convertToDPWDateFormat(shipmentDetails.getCarrierDetails().getEtd(), tsDateTimeFormat));
        dictionary.put(ETA_CAPS, convertToDPWDateFormat(shipmentDetails.getCarrierDetails().getEta(), tsDateTimeFormat));
        if (!isStringNullOrEmpty(shipmentDetails.getCarrierDetails().getShippingLine()) && carrierMasterDataMap.containsKey(shipmentDetails.getCarrierDetails().getShippingLine())) {
            String carrierCode = carrierMasterDataMap.get(shipmentDetails.getCarrierDetails().getShippingLine()).getIataCode();
            if (isStringNullOrEmpty(carrierCode))
                carrierCode = carrierMasterDataMap.get(shipmentDetails.getCarrierDetails().getShippingLine()).getItemValue();
            dictionary.put(CARRIER_CODE, carrierCode);
            dictionary.put(CARRIER_NAME, carrierMasterDataMap.get(shipmentDetails.getCarrierDetails().getShippingLine()).getItemDescription());
        }
        dictionary.put(FLIGHT_NUMBER1, shipmentDetails.getCarrierDetails().getFlightNumber());
        if (!isStringNullOrEmpty(shipmentDetails.getCarrierDetails().getOriginPort()) && unLocMap.containsKey(shipmentDetails.getCarrierDetails().getOriginPort())) {
            dictionary.put(ReportConstants.POL, unLocMap.get(shipmentDetails.getCarrierDetails().getOriginPort()).getLocCode());
            dictionary.put(POL_NAME, unLocMap.get(shipmentDetails.getCarrierDetails().getOriginPort()).getName());
        }
        if (!isStringNullOrEmpty(shipmentDetails.getCarrierDetails().getDestinationPort()) && unLocMap.containsKey(shipmentDetails.getCarrierDetails().getDestinationPort())) {
            dictionary.put(ReportConstants.POD, unLocMap.get(shipmentDetails.getCarrierDetails().getDestinationPort()).getLocCode());
            dictionary.put(POD_NAME, unLocMap.get(shipmentDetails.getCarrierDetails().getDestinationPort()).getName());
        }
        dictionary.put(SHIPMENT_WEIGHT, shipmentDetails.getWeight());
        dictionary.put(SHIPMENT_WEIGHT_UNIT, shipmentDetails.getWeightUnit());
        dictionary.put(SHIPMENT_VOLUME, shipmentDetails.getVolume());
        dictionary.put(SHIPMENT_VOLUME_UNIT, shipmentDetails.getVolumeUnit());
        dictionary.put(REQUEST_DATE_TIME, convertToDPWDateFormat(convertDateToUserTimeZone(LocalDateTime.now(), MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME), null, false), tsDateTimeFormat));
        dictionary.put(BRANCH_TIME_ZONE, MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME));
    }

    public void populateDictionaryForEmailFromConsolidation(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                                            Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap) {
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String tsDateTimeFormat = v1TenantSettingsResponse.getDPWDateFormat();
        dictionary.put(SHIPMENT_CREATE_USER, shipmentDetails.getCreatedBy());
        dictionary.put(SHIPMENT_ASSIGNED_USER, shipmentDetails.getAssignedTo());
        if (!isStringNullOrEmpty(shipmentDetails.getAssignedTo()))
            dictionary.put(SHIPMENT_ASSIGNED_USER_WITH_SLASH, "/ " + shipmentDetails.getAssignedTo());
        dictionary.put(INTERBRANCH_SHIPMENT_NUMBER, getShipmentIdHyperLink(shipmentDetails.getShipmentId(), shipmentDetails.getId()));
        dictionary.put(INTERBRANCH_SHIPMENT_NUMBER_WITHOUT_LINK, shipmentDetails.getShipmentId());
        dictionary.put(Constants.CONSOLIDATION_NUMBER, consolidationDetails.getConsolidationNumber());
        dictionary.put(SOURCE_CONSOLIDATION_NUMBER, consolidationDetails.getConsolidationNumber());
        dictionary.put(MAWB_NUMBER, consolidationDetails.getMawb());
        dictionary.put(ETD_CAPS, convertToDPWDateFormat(consolidationDetails.getCarrierDetails().getEtd(), tsDateTimeFormat));
        dictionary.put(ETA_CAPS, convertToDPWDateFormat(consolidationDetails.getCarrierDetails().getEta(), tsDateTimeFormat));
        dictionary.put(LAT, consolidationDetails.getLatDate());
        if (!isStringNullOrEmpty(consolidationDetails.getCarrierDetails().getShippingLine()) && carrierMasterDataMap.containsKey(consolidationDetails.getCarrierDetails().getShippingLine())) {
            String carrierCode = carrierMasterDataMap.get(consolidationDetails.getCarrierDetails().getShippingLine()).getIataCode();
            if (isStringNullOrEmpty(carrierCode))
                carrierCode = carrierMasterDataMap.get(consolidationDetails.getCarrierDetails().getShippingLine()).getItemValue();
            dictionary.put(CARRIER_CODE, carrierCode);
            dictionary.put(CARRIER_NAME, carrierMasterDataMap.get(consolidationDetails.getCarrierDetails().getShippingLine()).getItemDescription());
        }
        dictionary.put(FLIGHT_NUMBER1, consolidationDetails.getCarrierDetails().getFlightNumber());
        if (!isStringNullOrEmpty(consolidationDetails.getCarrierDetails().getOriginPort()) && unLocMap.containsKey(consolidationDetails.getCarrierDetails().getOriginPort())) {
            dictionary.put(ReportConstants.POL, unLocMap.get(consolidationDetails.getCarrierDetails().getOriginPort()).getLocCode());
            dictionary.put(POL_NAME, unLocMap.get(consolidationDetails.getCarrierDetails().getOriginPort()).getName());
        }
        if (!isStringNullOrEmpty(consolidationDetails.getCarrierDetails().getDestinationPort()) && unLocMap.containsKey(consolidationDetails.getCarrierDetails().getDestinationPort())) {
            dictionary.put(ReportConstants.POD, unLocMap.get(consolidationDetails.getCarrierDetails().getDestinationPort()).getLocCode());
            dictionary.put(POD_NAME, unLocMap.get(consolidationDetails.getCarrierDetails().getDestinationPort()).getName());
        }
        dictionary.put(ALLOCATED_WEIGHT, Objects.nonNull(consolidationDetails.getAllocations()) ? consolidationDetails.getAllocations().getWeight() : null);
        dictionary.put(ALLOCATED_WEIGHT_UNIT, Objects.nonNull(consolidationDetails.getAllocations()) ? consolidationDetails.getAllocations().getWeightUnit() : null);
        dictionary.put(ALLOCATED_VOLUME, Objects.nonNull(consolidationDetails.getAllocations()) ? consolidationDetails.getAllocations().getVolume() : null);
        dictionary.put(ALLOCATED_VOLUME_UNIT, Objects.nonNull(consolidationDetails.getAllocations()) ? consolidationDetails.getAllocations().getVolumeUnit() : null);
        dictionary.put(REQUEST_DATE_TIME, convertToDPWDateFormat(convertDateToUserTimeZone(LocalDateTime.now(), MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME), null, false), tsDateTimeFormat));
        dictionary.put(BRANCH_TIME_ZONE, MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME));
    }

    public void getUnLocationsData(List<String> unLocGuids, Map<String, UnlocationsResponse> map) {
        if (unLocGuids == null || unLocGuids.isEmpty())
            return;
        Map<String, UnlocationsResponse> tempMap = masterDataUtils.getLocationData(new HashSet<>(unLocGuids));
        map.putAll(tempMap);
    }

    public void getCarriersData(List<String> carrierCodes, Map<String, CarrierMasterData> map) {
        if (carrierCodes == null || carrierCodes.isEmpty())
            return;
        Map<String, CarrierMasterData> tempMap = masterDataUtils.getCarriersData(new HashSet<>(carrierCodes));
        map.putAll(tempMap);
    }

    public String getShipmentIdHyperLink(String shipmentId, Long id) {
        String link = baseUrl + "/v2/shipments/edit/" + id;
        return HTML_HREF_TAG_PREFIX + link + "'>" + shipmentId + HTML_HREF_TAG_SUFFIX;
    }

    public String getTaskIdHyperLinkV3(String shipmentId, String taskUuid) {
        String link = baseUrl + "/v2/cr3/shipments/task/" + taskUuid;
        return HTML_HREF_TAG_PREFIX + link + "'>" + shipmentId + HTML_HREF_TAG_SUFFIX;
    }

    public String getTaskIdHyperLinkV2(String shipmentId, String taskGuid) {
        String link = baseUrl + "/v2/shipments/tasks/" + taskGuid;
        return HTML_HREF_TAG_PREFIX + link + "'>" + shipmentId + HTML_HREF_TAG_SUFFIX;
    }

    public String getConsolidationIdHyperLink(String consolidationId, Long id) {
        String link = baseUrl + "/v2/shipments/consolidations/edit/" + id;
        return HTML_HREF_TAG_PREFIX + link + "'>" + consolidationId + HTML_HREF_TAG_SUFFIX;
    }

    @SuppressWarnings("java:S5857")
    public String replaceTagsFromData(Map<String, Object> map, String val) {
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            if (!Objects.isNull(entry.getValue()) && !Objects.isNull(entry.getKey()))
                val = val.replace("{" + entry.getKey() + "}", entry.getValue().toString());
        }
        val = val.replaceAll("\\{.*?\\}", "");
        return val;
    }
    @SuppressWarnings("java:S5857")
    public String replaceDefaultTagsFromData(Map<String, Object> map, String val) {
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            if (Objects.nonNull(entry.getValue()) && Objects.nonNull(entry.getKey())) {
                boolean isEmptyDocument = (entry.getKey().equalsIgnoreCase(SUMMARY_DOCUMENTS) && entry.getValue().toString().isEmpty())
                        || (entry.getKey().equalsIgnoreCase(LIST_ALL_DOCUMENTS) && entry.getValue().toString().isEmpty());

                if (!isEmptyDocument) {
                    val = val.replace("{" + entry.getKey() + "}", entry.getKey() + ":" + entry.getValue().toString());
                }
            }
        }
        val = val.replaceAll("\\{.*?\\}", "");
        return val;
    }

    public void getEmailTemplate(Map<ShipmentRequestedType, EmailTemplatesRequest> response) {
        List<String> requests = new ArrayList<>(List.of(SHIPMENT_PULL_REQUESTED_EMAIL_TYPE, SHIPMENT_PULL_ACCEPTED_EMAIL_TYPE, SHIPMENT_PUSH_REJECTED_EMAIL_TYPE, SHIPMENT_PULL_REJECTED_EMAIL_TYPE,
                SHIPMENT_PUSH_REQUESTED_EMAIL_TYPE, SHIPMENT_PUSH_ACCEPTED_EMAIL_TYPE, SHIPMENT_DETACH_EMAIL_TYPE, SHIPMENT_PULL_WITHDRAW_EMAIL_TYPE, SHIPMENT_PUSH_WITHDRAW_EMAIL_TYPE));
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> field = new ArrayList<>(List.of(Constants.TYPE));
        String operator = Operators.IN.getValue();
        List<Object> criteria1 = new ArrayList<>(List.of(field, operator, List.of(requests)));
        List<Object> criteria2 = new ArrayList<>(List.of(List.of(TENANTID), "=", TenantContext.getCurrentTenant()));
        request.setCriteriaRequests(List.of(criteria1, "and", criteria2));
        V1DataResponse v1DataResponse = iv1Service.getEmailTemplates(request);
        if (v1DataResponse != null) {
            List<EmailTemplatesRequest> emailTemplatesRequests = jsonHelper.convertValueToList(v1DataResponse.entities, EmailTemplatesRequest.class);
            if (emailTemplatesRequests != null && !emailTemplatesRequests.isEmpty()) {
                populateEmailTemplateInResponseMap(response, emailTemplatesRequests);
            }
        }
    }

    private void populateEmailTemplateInResponseMap(Map<ShipmentRequestedType, EmailTemplatesRequest> response, List<EmailTemplatesRequest> emailTemplatesRequests) {
        for (EmailTemplatesRequest emailTemplatesRequest : emailTemplatesRequests) {
            ShipmentRequestedType shipmentRequestedType = EMAIL_TYPE_MAPPING.get(emailTemplatesRequest.getType());
            if (shipmentRequestedType != null) {
                response.put(shipmentRequestedType, emailTemplatesRequest);
            }
        }
    }

    public void getToAndCCEmailIdsFromTenantSettingsAndTenantsData(Set<Integer> tenantIds, Map<Integer, V1TenantSettingsResponse> tenantSettingsMap, Map<Integer, TenantModel> tenantsModelMap) {
        Map<Integer, Object> map = getTenantSettingsAndTenantsData(tenantIds);
        map.forEach((key, value) -> {
            TenantDetailsByListResponse.TenantDetails tenantDetails = modelMapper.map(value, TenantDetailsByListResponse.TenantDetails.class);
            tenantSettingsMap.put(key, modelMapper.map(tenantDetails.getTenantSettings(), V1TenantSettingsResponse.class));
            tenantsModelMap.put(key, modelMapper.map(tenantDetails.getTenant(), TenantModel.class));
        });
    }

    public Map<Integer, Object> getTenantSettingsAndTenantsData(Set<Integer> tenantIds) {
        if (tenantIds.isEmpty())
            return new HashMap<>();

        try {
            var v1Response = iv1Service.getTenantDetails(TenantDetailsByListRequest.builder().tenantIds(tenantIds.stream().toList()).take(100).build());
            return v1Response.getEntities()
                    .stream()
                    .collect(Collectors.groupingBy(
                            TenantDetailsByListResponse.TenantDetails::getTenantId,
                            Collectors.collectingAndThen(
                                    Collectors.toList(),
                                    list -> list.get(0)
                            )));
        } catch (Exception ex) {
            log.error(ex.getMessage());
            return new HashMap<>();
        }
    }

    public void getToAndCCEmailIdsFromTenantSettings(Set<Integer> tenantIds, Map<Integer, V1TenantSettingsResponse> tenantSettingsMap) {
        Map<Integer, Object> map = getTenantSettings(new ArrayList<>(tenantIds));
        map.forEach((key, value) -> tenantSettingsMap.put(key, modelMapper.map(value, V1TenantSettingsResponse.class)));
    }

    public Map<Integer, Object> getTenantSettings(List<Integer> tenantIds) {
        if (tenantIds.isEmpty())
            return new HashMap<>();

        try {
            var v1Response = iv1Service.getTenantDetails(TenantDetailsByListRequest.builder().tenantIds(tenantIds).take(100).build());
            return v1Response.getEntities()
                    .stream()
                    .collect(Collectors.groupingBy(
                            TenantDetailsByListResponse.TenantDetails::getTenantId,
                            Collectors.collectingAndThen(
                                    Collectors.toList(),
                                    list -> list.get(0).getTenantSettings()
                            )));
        } catch (Exception ex) {
            log.error(ex.getMessage());
            return new HashMap<>();
        }
    }

    public void getUserDetails(Set<String> usernamesList, Map<String, String> usernameEmailsMap) {
        usernamesList.remove(null);
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> field = new ArrayList<>(List.of("Username"));
        String operator = Operators.IN.getValue();
        List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(usernamesList)));
        request.setCriteriaRequests(criteria);
        request.setTake(usernamesList.size());
        V1DataResponse v1DataResponse = iv1Service.getUserDetails(request);
        List<UsersDto> usersDtos = jsonHelper.convertValueToList(v1DataResponse.entities, UsersDto.class);
        usernameEmailsMap.putAll(usersDtos.stream()
                .filter(user -> !isStringNullOrEmpty(user.getEmail()))
                .collect(Collectors.toMap(UsersDto::getUsername, UsersDto::getEmail)));
    }

    // called when new dg pack is added or dg pack fields are changed or new dg container is added, or new pack added in dg container or dg container fields are changed
    public boolean changeShipmentDGStatusToReqd(ShipmentDetails shipmentDetails, boolean isDGClass1) {
        OceanDGStatus oldOceanDGStatus = shipmentDetails.getOceanDGStatus();
        if (Constants.IMP.equals(shipmentDetails.getDirection())) {
            shipmentDetails.setOceanDGStatus(null);
            return false;
        }

        if (Objects.isNull(shipmentDetails.getOceanDGStatus()) ||
                (!UserContext.isOceanDgUser() && (OceanDGStatus.OCEAN_DG_ACCEPTED.equals(shipmentDetails.getOceanDGStatus()) ||
                        OceanDGStatus.OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED.equals(shipmentDetails.getOceanDGStatus()) ||
                        OceanDGStatus.OCEAN_DG_COMMERCIAL_REJECTED.equals(shipmentDetails.getOceanDGStatus()) ||
                        OceanDGStatus.OCEAN_DG_COMMERCIAL_ACCEPTED.equals(shipmentDetails.getOceanDGStatus()))))
            shipmentDetails.setOceanDGStatus(OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED);

        if ((OceanDGStatus.OCEAN_DG_COMMERCIAL_ACCEPTED.equals(shipmentDetails.getOceanDGStatus()) && !UserContext.isOceanDgCommercialUser()) ||
                (isDGClass1 && OceanDGStatus.OCEAN_DG_ACCEPTED.equals(shipmentDetails.getOceanDGStatus()))) {
            shipmentDetails.setOceanDGStatus(OceanDGStatus.OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED);
        }
        return !Objects.equals(oldOceanDGStatus, shipmentDetails.getOceanDGStatus());
    }

    public boolean checkIfDGClass1(String dgClass) {
        return !isStringNullOrEmpty(dgClass) && dgClass.charAt(0) == '1';
    }

    public boolean checkIfAnyDGClass(String dgClass) throws RunnerException {
        if (!isStringNullOrEmpty(dgClass)) {
            if (dgClass.charAt(0) == '7')
                throw new RunnerException("As per the DG SOP, you are not allowed to deal in Class 7 DG shipments");
            return true;
        }
        return false;
    }


    public boolean checkIfDGFieldsChangedInPacking(PackingRequest newPack, Packing oldPack) {
        return isDGChanged(
                newPack.getHazardous(), oldPack.getHazardous(),
                newPack.getDGClass(), oldPack.getDGClass(),
                newPack.getUnNumber(), oldPack.getUnNumber(),
                newPack.getProperShippingName(), oldPack.getProperShippingName(),
                newPack.getPackingGroup(), oldPack.getPackingGroup(),
                newPack.getMinimumFlashPoint(), oldPack.getMinimumFlashPoint(),
                newPack.getMinimumFlashPointUnit(), oldPack.getMinimumFlashPointUnit(),
                newPack.getMarinePollutant(), oldPack.getMarinePollutant()
        );
    }

    public boolean checkIfDGFieldsChangedInPackingV3(Packing newPack, Packing oldPack) {
        return isDGChanged(
                newPack.getHazardous(), oldPack.getHazardous(),
                newPack.getDGClass(), oldPack.getDGClass(),
                newPack.getUnNumber(), oldPack.getUnNumber(),
                newPack.getProperShippingName(), oldPack.getProperShippingName(),
                newPack.getPackingGroup(), oldPack.getPackingGroup(),
                newPack.getMinimumFlashPoint(), oldPack.getMinimumFlashPoint(),
                newPack.getMinimumFlashPointUnit(), oldPack.getMinimumFlashPointUnit(),
                newPack.getMarinePollutant(), oldPack.getMarinePollutant()
        );
    }

    private boolean isDGChanged(Boolean newHaz, Boolean oldHaz,
                                String newClass, String oldClass,
                                String newUn, String oldUn,
                                String newName, String oldName,
                                String newGroup, String oldGroup,
                                BigDecimal newFlash, BigDecimal oldFlash,
                                String newFlashUnit, String oldFlashUnit,
                                Boolean newPollutant, Boolean oldPollutant) {
        if (!Objects.equals(oldHaz, newHaz)) return true;
        if (!Objects.equals(newClass, oldClass)) return true;
        if (!Objects.equals(newUn, oldUn)) return true;
        if (!Objects.equals(newName, oldName)) return true;
        if (!Objects.equals(newGroup, oldGroup)) return true;
        if (!compareBigDecimals(newFlash, oldFlash)) return true;
        if (!Objects.equals(newFlashUnit, oldFlashUnit)) return true;
        return !Objects.equals(oldPollutant, newPollutant);
    }


    public boolean compareBigDecimals(BigDecimal bd1, BigDecimal bd2) {
        // Check if both are null, they are considered equal
        if (Objects.equals(bd1, bd2)) {
            return true;
        }
        // If one is null and the other is not, they are not equal
        if (bd1 == null || bd2 == null) {
            return false;
        }
        // Use compareTo to ignore scale differences (0.00 vs 0.0)
        return bd1.compareTo(bd2) == 0;
    }

    public boolean checkIfDGFieldsChangedInContainer(ContainerRequest newContainer, Containers oldContainer) {
        if (!oldContainer.getHazardous().equals(newContainer.getHazardous()))
            return true;
        if (!Objects.equals(newContainer.getDgClass(), oldContainer.getDgClass()))
            return true;
        if (!Objects.equals(newContainer.getUnNumber(), oldContainer.getUnNumber()))
            return true;
        if (!Objects.equals(newContainer.getProperShippingName(), oldContainer.getProperShippingName()))
            return true;
        if (!Objects.equals(newContainer.getPackingGroup(), oldContainer.getPackingGroup()))
            return true;
        if (!compareBigDecimals(newContainer.getMinimumFlashPoint(), oldContainer.getMinimumFlashPoint()))
            return true;
        if (!Objects.equals(newContainer.getMinimumFlashPointUnit(), oldContainer.getMinimumFlashPointUnit()))
            return true;
        return !oldContainer.getMarinePollutant().equals(newContainer.getMarinePollutant());
    }

    public boolean checkIfDGFieldsChangedInContainer(ContainerV3Request newContainer, Containers oldContainer) {
        if (!oldContainer.getHazardous().equals(newContainer.getHazardous()))
            return true;
        if (!Objects.equals(newContainer.getDgClass(), oldContainer.getDgClass()))
            return true;
        if (!Objects.equals(newContainer.getUnNumber(), oldContainer.getUnNumber()))
            return true;
        if (!Objects.equals(newContainer.getProperShippingName(), oldContainer.getProperShippingName()))
            return true;
        if (!Objects.equals(newContainer.getPackingGroup(), oldContainer.getPackingGroup()))
            return true;
        if (!compareBigDecimals(newContainer.getMinimumFlashPoint(), oldContainer.getMinimumFlashPoint()))
            return true;
        if (!Objects.equals(newContainer.getMinimumFlashPointUnit(), oldContainer.getMinimumFlashPointUnit()))
            return true;
        return !oldContainer.getMarinePollutant().equals(newContainer.getMarinePollutant());
    }

    public void updateUnLocData(CarrierDetails carrierDetails, CarrierDetails oldCarrierDetails) {
        try {
            if (!Objects.isNull(carrierDetails) && (Objects.isNull(oldCarrierDetails) || !Objects.equals(carrierDetails.getOrigin(), oldCarrierDetails.getOrigin())
                    || !Objects.equals(carrierDetails.getOriginPort(), oldCarrierDetails.getOriginPort())
                    || !Objects.equals(carrierDetails.getDestination(), oldCarrierDetails.getDestination())
                    || !Objects.equals(carrierDetails.getDestinationPort(), oldCarrierDetails.getDestinationPort()))) {
                Set<String> unlocoRequests = getUnlocoRequests(carrierDetails);
                Map<String, EntityTransferUnLocations> unlocationsMap = masterDataUtils.getLocationDataFromCache(unlocoRequests, EntityTransferConstants.LOCATION_SERVICE_GUID);
                EntityTransferUnLocations pol = unlocationsMap.get(carrierDetails.getOriginPort());
                EntityTransferUnLocations pod = unlocationsMap.get(carrierDetails.getDestinationPort());
                EntityTransferUnLocations origin = unlocationsMap.get(carrierDetails.getOrigin());
                EntityTransferUnLocations destination = unlocationsMap.get(carrierDetails.getDestination());
                if (!Objects.isNull(origin))
                    carrierDetails.setOriginLocCode(origin.getLocCode());
                if (!Objects.isNull(destination))
                    carrierDetails.setDestinationLocCode(destination.getLocCode());
                if (!Objects.isNull(pol))
                    carrierDetails.setOriginPortLocCode(pol.getLocCode());
                if (!Objects.isNull(pod))
                    carrierDetails.setDestinationPortLocCode(pod.getLocCode());
            }
        } catch (Exception e) {
            log.error("Error while updating unlocCode for Carrier with Id {} due to {}", carrierDetails.getId(), e.getMessage());
        }
    }

    private Set<String> getUnlocoRequests(CarrierDetails carrierDetails) {
        Set<String> unlocoRequests = new HashSet<>();
        if (!isStringNullOrEmpty(carrierDetails.getOrigin()))
            unlocoRequests.add(carrierDetails.getOrigin());
        if (!isStringNullOrEmpty(carrierDetails.getOriginPort()))
            unlocoRequests.add(carrierDetails.getOriginPort());
        if (!isStringNullOrEmpty(carrierDetails.getDestination()))
            unlocoRequests.add(carrierDetails.getDestination());
        if (!isStringNullOrEmpty(carrierDetails.getDestinationPort()))
            unlocoRequests.add(carrierDetails.getDestinationPort());
        return unlocoRequests;
    }

    public void updateCarrierUnLocData(CarrierDetails carrierDetails, Map<String, EntityTransferUnLocations> unlocationsMap) {
        try {
            EntityTransferUnLocations pol = unlocationsMap.get(carrierDetails.getOriginPort());
            EntityTransferUnLocations pod = unlocationsMap.get(carrierDetails.getDestinationPort());
            EntityTransferUnLocations origin = unlocationsMap.get(carrierDetails.getOrigin());
            EntityTransferUnLocations destination = unlocationsMap.get(carrierDetails.getDestination());
            if (!Objects.isNull(origin))
                carrierDetails.setOriginLocCode(origin.getLocCode());
            if (!Objects.isNull(destination))
                carrierDetails.setDestinationLocCode(destination.getLocCode());
            if (!Objects.isNull(pol))
                carrierDetails.setOriginPortLocCode(pol.getLocCode());
            if (!Objects.isNull(pod))
                carrierDetails.setDestinationPortLocCode(pod.getLocCode());
        } catch (Exception e) {
            log.error("Error while updating unlocCode for Carrier with Id {} due to {}", carrierDetails.getId(), e.getMessage());
        }
    }

    public void updateRoutingUnLocData(List<Routings> routingsList, Map<String, EntityTransferUnLocations> unlocationsMap) {
        try {
            if(Objects.isNull(routingsList)) {
                return ;
            }
            for (var routing : routingsList) {
                EntityTransferUnLocations pol = unlocationsMap.get(routing.getPol());
                EntityTransferUnLocations pod = unlocationsMap.get(routing.getPod());
                if (!Objects.isNull(pol))
                    routing.setOriginPortLocCode(pol.getLocCode());
                if (!Objects.isNull(pod))
                    routing.setDestinationPortLocCode(pod.getLocCode());
            }
        } catch (Exception e) {
            log.error("Error while updating un-locCode for routing list {}", e.getMessage());
        }
    }

    public <T> void getChangedUnLocationFields(T newEntity, T oldEntity, Set<String> unlocationSet) {
        if (Objects.isNull(newEntity))
            return;
        Class<?> clazz = newEntity.getClass();
        try {
            for (Field field : clazz.getDeclaredFields()) {
                field.setAccessible(true);
                if (field.isAnnotationPresent(UnlocationData.class) && (Objects.isNull(oldEntity) || !Objects.equals(field.get(newEntity), field.get(oldEntity))) && !Objects.isNull(field.get(newEntity))) {
                    unlocationSet.add((String) field.get(newEntity));
                }
            }
        } catch (Exception e) {
            log.warn("Error while getting un-location fields for class {}", clazz.getSimpleName());
        }
    }

    public <T extends BaseEntity> void getChangedUnLocationFields(List<T> newEntityList, List<T> oldEntityList, Set<String> unlocationSet) {
        if (CollectionUtils.isEmpty(newEntityList))
            return;

        Map<Long, T> oldEntityMap = Optional.ofNullable(oldEntityList).orElse(Collections.emptyList()).stream().collect(Collectors.toMap(BaseEntity::getId, Function.identity()));
        for (T newEntity : newEntityList) {
            getChangedUnLocationFields(newEntity, oldEntityMap.get(newEntity.getId()), unlocationSet);
        }
    }

    public String convertToDPWDateFormat(LocalDateTime date, String tsDatetimeFormat) {
        String strDate = "";
        if (date != null) {
            if (!isStringNullOrEmpty(tsDatetimeFormat))
                strDate = date.format(DateTimeFormatter.ofPattern(tsDatetimeFormat));
            else
                strDate = date.format(getDPWDateFormatOrDefault());
        }
        return strDate;
    }

    public DateTimeFormatter getDPWDateFormatOrDefault() {
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        if (!CommonUtils.isStringNullOrEmpty(v1TenantSettingsResponse.getDPWDateFormat()))
            return DateTimeFormatter.ofPattern(v1TenantSettingsResponse.getDPWDateFormat());
        return DateTimeFormatter.ofPattern("MM/dd/yyyy");
    }

    public static double roundOffAirShipment(double charge) {
        if (charge - 0.50 <= Math.floor(charge) && charge != Math.floor(charge)) {
            charge = Math.floor(charge) + 0.5;
        } else {
            charge = Math.ceil(charge);
        }
        return charge;
    }

    public void populateDictionaryForOceanDGApproval(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, VesselsResponse vesselsResponse, String remarks,
                                                     TaskCreateResponse taskCreateResponse, boolean taskServiceV2Enabled) {

        populateDictionaryForDGEmailFromShipment(dictionary, shipmentDetails, vesselsResponse, taskCreateResponse);
        populateDictionaryApprovalRequestForDGEmail(dictionary, remarks);
        if(taskServiceV2Enabled){
            dictionary.put(VIEWS, getTaskIdHyperLinkV2(shipmentDetails.getShipmentId(), taskCreateResponse.getTaskGuid()));
        }else {
            dictionary.put(VIEWS, getTaskIdHyperLinkV2(shipmentDetails.getShipmentId(), taskCreateResponse.getTasksId()));
        }
    }

    public void populateDictionaryForOceanDGCommercialApproval(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, VesselsResponse vesselsResponse, String remarks, TaskCreateResponse taskCreateResponse, boolean taskServiceV2Enabled) {
        populateDictionaryForOceanDGApproval(dictionary, shipmentDetails, vesselsResponse, remarks, taskCreateResponse, taskServiceV2Enabled);
        List<AuditLog> auditLogList = iAuditLogDao.findByOperationAndParentId(
                DBOperationType.DG_APPROVE.name(), shipmentDetails.getId());
        if(auditLogList != null && !auditLogList.isEmpty()){
            Map<String, AuditLogChanges> changesMap = auditLogList.get(0).getChanges();
            populateDGSenderDetailsFromAudit(changesMap, dictionary);
        }
    }

    public void getDGEmailTemplate(Map<OceanDGStatus, EmailTemplatesRequest> response) {
        List<String> requests = new ArrayList<>(
                List.of(OCEAN_DG_APPROVAL_REQUEST_EMAIL_TYPE, OCEAN_DG_APPROVAL_APPROVE_EMAIL_TYPE, OCEAN_DG_APPROVAL_REJECTION_EMAIL_TYPE,
                        OCEAN_DG_COMMERCIAL_APPROVAL_REQUEST_EMAIL_TYPE, OCEAN_DG_COMMERCIAL_APPROVAL_APPROVE_EMAIL_TYPE,
                        OCEAN_DG_COMMERCIAL_APPROVAL_REJECTION_EMAIL_TYPE));

        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> field = new ArrayList<>(List.of(Constants.TYPE));
        String operator = Operators.IN.getValue();
        List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
        request.setCriteriaRequests(criteria);
        V1DataResponse v1DataResponse = iv1Service.getEmailTemplates(request);
        if (v1DataResponse != null && v1DataResponse.entities != null) {
            List<EmailTemplatesRequest> emailTemplates = jsonHelper.convertValueToList(v1DataResponse.entities, EmailTemplatesRequest.class);

            if (emailTemplates != null && !emailTemplates.isEmpty()) {
                emailTemplates.stream()
                        .filter(Objects::nonNull)
                        .forEach(template -> {
                            switch (template.getType()) {
                                case OCEAN_DG_APPROVAL_REQUEST_EMAIL_TYPE:
                                    response.put(OCEAN_DG_REQUESTED, template);
                                    break;
                                case OCEAN_DG_APPROVAL_APPROVE_EMAIL_TYPE:
                                    response.put(OCEAN_DG_ACCEPTED, template);
                                    break;
                                case OCEAN_DG_APPROVAL_REJECTION_EMAIL_TYPE:
                                    response.put(OCEAN_DG_REJECTED, template);
                                    break;
                                case OCEAN_DG_COMMERCIAL_APPROVAL_REQUEST_EMAIL_TYPE:
                                    response.put(OCEAN_DG_COMMERCIAL_REQUESTED, template);
                                    break;
                                case OCEAN_DG_COMMERCIAL_APPROVAL_APPROVE_EMAIL_TYPE:
                                    response.put(OCEAN_DG_COMMERCIAL_ACCEPTED, template);
                                    break;
                                case OCEAN_DG_COMMERCIAL_APPROVAL_REJECTION_EMAIL_TYPE:
                                    response.put(OCEAN_DG_COMMERCIAL_REJECTED, template);
                                    break;
                                default:
                                    break;
                            }
                        });
            }
        }
    }

    public Integer getRoleId(OceanDGStatus oceanDGStatus) {
        String roleName = oceanDGStatus == OCEAN_DG_REQUESTED ? OCEAN_DG_ROLE : COMMERCIAL_OCEAN_DG_ROLE;
        return getRoleIDByRoleName(roleName, UserContext.getUser().getTenantId());
    }

    private Integer getRoleIDByRoleName(String roleName, Integer tenantId) {
        V1RoleIdRequest v1RoleIdRequest = V1RoleIdRequest
                .builder()
                .roleName(roleName)
                .tenantId(tenantId)
                .build();
        return iv1Service.getRoleIdsByRoleName(v1RoleIdRequest);
    }

    public List<String> getUserEmailsByRoleId(Integer roleId) {
        V1UsersEmailRequest request = new V1UsersEmailRequest();
        request.setRoleId(roleId);
        request.setTake(10);
        List<String> userEmailIds = new ArrayList<>();
        List<UsersRoleListResponse> userEmailResponse = iv1Service.getUserEmailsByRoleId(request);
        userEmailResponse.forEach(e -> userEmailIds.add(e.getEmail()));

        return userEmailIds;
    }

    public TaskCreateResponse createTask(ShipmentDetails shipmentDetails, Integer roleId)
            throws RunnerException {
        DGTaskCreateRequest taskRequest = DGTaskCreateRequest
                .builder()
                .entityType(SHIPMENTS_WITH_SQ_BRACKETS)
                .entityId(shipmentDetails.getId().toString())
                .roleId(roleId.toString())
                .taskType(OCEAN_DG_TASKTYPE)
                .taskStatus(PENDING_ACTION)
                .userId(UserContext.getUser().getUserId())
                .tenantId(UserContext.getUser().getTenantId().toString())
                .build();

        try {
            return iv1Service.createTask(taskRequest);
        } catch (Exception e) {
            throw new RunnerException(String.format("Task creation failed for shipmentId: %s. Error: %s",
                    shipmentDetails.getId(), e.getMessage()));
        }
    }

    public TaskCreateResponse createTaskMDM(ShipmentDetails shipmentDetails, Integer roleId) throws RunnerException {
        MdmTaskCreateRequest taskRequest = MdmTaskCreateRequest
            .builder()
            .entityType(SHIPMENTS_WITH_SQ_BRACKETS)
            .entityUuid(shipmentDetails.getGuid().toString())
            .roleId(roleId)
            .taskType(DG_OCEAN_APPROVAL)
            .status(PENDING_ACTION_TASK)
            .userId(UserContext.getUser().getUserId())
            .isCreatedFromV2(true)
            .sendEmail(false)
            .build();

        try {
            com.dpw.runner.shipment.services.dto.response.mdm.MdmTaskCreateResponse mdmTaskCreateResponse =  mdmServiceAdapter.createTask(taskRequest);
            return TaskCreateResponse
                .builder()
                .tasksId(mdmTaskCreateResponse.getId().toString())
                .taskGuid(mdmTaskCreateResponse.getUuid())
                .build();
        } catch (Exception e) {
            throw new RunnerException(String.format("Task creation failed for shipmentId: %s. Error: %s",
                shipmentDetails.getId(), e.getMessage()));
        }
    }

    public void getVesselsData(CarrierDetails carrierDetails, VesselsResponse vesselsResponse) {
        if (carrierDetails == null) return;
        String guid = carrierDetails.getVessel();
        if (isStringNullOrEmpty(guid)) {
            return;
        }
        List<Object> vesselCriteria = Arrays.asList(
                List.of(Constants.VESSEL_GUID_V1),
                "=",
                guid
        );
        CommonV1ListRequest vesselRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(vesselCriteria).build();
        V1DataResponse v1DataResponse = iv1Service.fetchVesselData(vesselRequest);
        List<VesselsResponse> vesselsResponseList = jsonHelper.convertValueToList(v1DataResponse.entities, VesselsResponse.class);

        if (vesselsResponseList != null && !vesselsResponseList.isEmpty()) {
            vesselsResponse.setName(vesselsResponseList.get(0).getName());
        }

    }

    private void populateDictionaryForDGEmailFromShipment(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, VesselsResponse vesselsResponse, TaskCreateResponse taskCreateResponse) {
        if (shipmentDetails.getCarrierDetails() != null) {
            dictionary.put(ORIGIN_PORT, shipmentDetails.getCarrierDetails().getOriginPort());
            dictionary.put(DESTINATION_PORT, shipmentDetails.getCarrierDetails().getDestinationPort());
            dictionary.put(CARRIER, shipmentDetails.getCarrierDetails().getShippingLine());
            dictionary.put(VOYAGE, shipmentDetails.getCarrierDetails().getVoyage());
            if (shipmentDetails.getCarrierDetails().getEta() != null) {
                dictionary.put(ETA, shipmentDetails.getCarrierDetails().getEta().format(DateTimeFormatter.ofPattern(DATE_TIME_FORMAT)));
            }

            if (shipmentDetails.getCarrierDetails().getEtd() != null) {
                dictionary.put(ETD, shipmentDetails.getCarrierDetails().getEtd()
                        .format(DateTimeFormatter.ofPattern(DATE_TIME_FORMAT)));
            }
        }
        dictionary.put(TRANSPORT_MODE, shipmentDetails.getTransportMode());
        dictionary.put(SHIPMENT_TYPE, shipmentDetails.getDirection());
        dictionary.put(SHIPMENT_NUMBER, shipmentDetails.getShipmentId());
        dictionary.put(CARGO_TYPE, shipmentDetails.getShipmentType());
        if (vesselsResponse != null) {
            dictionary.put(VESSEL_NAME, vesselsResponse.getName());
        }


        //Summary of cargo Details
        long totalContainerCount = shipmentDetails.getContainersList().stream()
                .mapToLong(Containers::getContainerCount)
                .sum();

        long dgContainerCount = shipmentDetails.getContainersList().stream()
                .filter(Containers::getHazardous)
                .mapToLong(Containers::getContainerCount)
                .sum();

        dictionary.put(CONTAINER_COUNT, totalContainerCount);
        dictionary.put(DG_CONTAINER_COUNT, dgContainerCount);


        String dgPackageTypeAndCount = Optional.ofNullable(shipmentDetails.getPackingList())
                .orElse(List.of())  // If the list is null, use an empty list
                .stream()
                .filter(Packing::getHazardous)
                .map(packing -> packing.getPacks() + " " + packing.getPacksType())
                .collect(Collectors.joining(", "));

        String packagesTypeAndCount = Optional.ofNullable(shipmentDetails.getPackingList())
                .orElse(List.of())  // If the list is null, use an empty list
                .stream()
                .map(packing -> packing.getPacks() + " " + packing.getPacksType())
                .collect(Collectors.joining(", "));

        dictionary.put(DG_PACKAGES_TYPE, dgPackageTypeAndCount);
        dictionary.put(TOTAL_PACKAGES_TYPE, packagesTypeAndCount);
    }

    private void populateDictionaryApprovalRequestForDGEmail(Map<String, Object> dictionary, String remarks) {
        dictionary.put(USER_BRANCH, UserContext.getUser().getTenantDisplayName());
        dictionary.put(USER_COUNTRY, UserContext.getUser().getTenantCountryCode());
        dictionary.put(USER_NAME, UserContext.getUser().getUsername());
        dictionary.put(REQUEST_DATE_TIME, LocalDateTime.now().format(DateTimeFormatter.ofPattern(DATE_TIME_FORMAT)));
        dictionary.put(REQUESTER_REMARKS, remarks);
    }

    private void populateDGReceiverDictionary(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, OceanDGRequest request) {
        dictionary.put(USER_BRANCH, UserContext.getUser().getTenantDisplayName());
        dictionary.put(USER_COUNTRY, UserContext.getUser().getTenantCountryCode());
        dictionary.put(SHIPMENT_NUMBER, shipmentDetails.getShipmentId());
        dictionary.put(APPROVER_NAME, UserContext.getUser().getUsername());
        dictionary.put(APPROVED_TIME, LocalDateTime.now().format(DateTimeFormatter.ofPattern(DATE_TIME_FORMAT)));
        dictionary.put(REMARKS, request.getRemarks());
        dictionary.put(STATUS, request.getStatus());
    }

    private void populateDGReceiverDictionaryV3(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, OceanDGRequestV3 request) {
        dictionary.put(USER_BRANCH, UserContext.getUser().getTenantDisplayName());
        dictionary.put(USER_COUNTRY, UserContext.getUser().getTenantCountryCode());
        dictionary.put(SHIPMENT_NUMBER, shipmentDetails.getShipmentId());
        dictionary.put(APPROVER_NAME, UserContext.getUser().getUsername());
        dictionary.put(APPROVED_TIME, LocalDateTime.now().format(DateTimeFormatter.ofPattern(DATE_TIME_FORMAT)));
        dictionary.put(REMARKS, request.getRemarks());
        dictionary.put(STATUS, request.getStatus());
    }

    private void populateDGSenderDetailsFromAudit(Map<String, AuditLogChanges> changesMap, Map<String, Object> dictionary) {

        for (AuditLogChanges change : changesMap.values()) {
            if (change.getFieldName().equalsIgnoreCase(TIME)) {
                dictionary.put(DG_APPROVER_TIME, change.getNewValue());
            } else if (change.getFieldName().equalsIgnoreCase(USERNAME)) {
                dictionary.put(DG_APPROVER_NAME, change.getNewValue());
            }
        }
    }

    public void removeDuplicateTrackingEvents(List<Events> events) {
        if (events == null || events.isEmpty()) {
            return;
        }

        Set<String> uniqueKeys = new HashSet<>();

        events.removeIf(event -> !uniqueKeys.add(
                getTrackingEventsUniqueKey(
                        event.getEventCode(),
                        event.getContainerNumber(),
                        event.getShipmentNumber(),
                        event.getSource(),
                        event.getPlaceName()
                )
        ));
    }

    public String getTrackingEventsUniqueKey(String eventCode, String containerNumber, String shipmentNumber, String source, String placeName) {
        return String.join("-",
                StringUtils.defaultString(eventCode),
                StringUtils.defaultString(containerNumber),
                StringUtils.defaultString(shipmentNumber),
                StringUtils.defaultString(source),
                StringUtils.defaultString(placeName)
        );
    }

    public static String getKey(String... args) {
        return String.join("-", args);
    }

    // This method is used to validate whether the transport mode is allowed to be used or not
    public boolean isTransportModeValid(String transportMode, String entity, V1TenantSettingsResponse tenantSettings) {
        Set<String> validTransportModes;

        switch (entity) {
            case CUSTOMER_BOOKING ->
                    validTransportModes = this.getValidTransportModes(tenantSettings.getBookingTransportModeAir(), tenantSettings.getBookingTransportModeSea(), tenantSettings.getBookingTransportModeRail(), tenantSettings.getBookingTransportModeRoad());
            case SHIPMENT_DETAILS ->
                    validTransportModes = this.getValidTransportModes(tenantSettings.getShipmentTransportModeAir(), tenantSettings.getShipmentTransportModeSea(), tenantSettings.getShipmentTransportModeRail(), tenantSettings.getShipmentTransportModeRoad());
            default -> validTransportModes = new HashSet<>();
        }

        return validTransportModes.contains(transportMode);
    }

    /**
     * This method will return list of valid transport modes based on the tenant configs
     *
     * @param isAirValid  if true, add AIR in valid modes list
     * @param isSeaValid  if true, add SEA in valid modes list
     * @param isRailValid if true, add RAI in valid modes list
     * @param isRoadValid if true, add ROA in valid modes list
     * @return Set of valid transport modes
     */
    private Set<String> getValidTransportModes(Boolean isAirValid, Boolean isSeaValid, Boolean isRailValid, Boolean isRoadValid) {

        Set<String> transportModes = new HashSet<>();

        if (Boolean.TRUE.equals(isAirValid))
            transportModes.add(Constants.TRANSPORT_MODE_AIR);

        if (Boolean.TRUE.equals(isSeaValid))
            transportModes.add(Constants.TRANSPORT_MODE_SEA);

        if (Boolean.TRUE.equals(isRailValid))
            transportModes.add(Constants.TRANSPORT_MODE_RAI);

        if (Boolean.TRUE.equals(isRoadValid))
            transportModes.add(Constants.TRANSPORT_MODE_ROA);

        return transportModes;
    }

    public void createMasterDataKeysList(Set<MasterListRequest> masterListRequests, Set<String> keys) {
        if (Objects.isNull(masterListRequests))
            return;
        for (MasterListRequest masterListRequest : masterListRequests) {
            keys.add(masterListRequest.ItemValue + '#' + MasterDataType.getNameFromDescription(masterListRequest.ItemType));
        }
    }

    /**
     * Updates the given list of events with description and direction from master data.
     *
     * <p>Filters events without IDs and with non-null event codes, fetches corresponding
     * master data, and updates the description and direction if matching data is found.
     * Preserves original values if no match is available.</p>
     *
     * @param eventsList the list of {@link Events} to update; does nothing if null or empty.
     */
    public void updateEventWithMasterData(List<Events> eventsList) {
        if (CollectionUtils.isEmpty(eventsList))
            return;
        // Create a map of event codes to their corresponding master data entries.
        // Filter out events with non-null IDs and null event codes before mapping.
        Map<String, EntityTransferMasterLists> eventCodeMasterDataMap = getEventCodeMasterDataMap(eventsList.stream()
                .filter(i -> Objects.isNull(i.getId()))
                .map(Events::getEventCode)
                .filter(Objects::nonNull).toList());

        eventsList.forEach(event -> {
            EntityTransferMasterLists masterData = eventCodeMasterDataMap.get(event.getEventCode());
            // If master data is found, update the event's description and direction.
            if (masterData != null) {
                event.setDescription(masterData.getItemDescription());
                if (masterData.getIdentifier3() != null) {
                    event.setDirection(masterData.getIdentifier3());
                }
            }
            // If no master data is found, retain the original description and direction.
        });
    }

    private Map<String, EntityTransferMasterLists> getEventCodeMasterDataMap(List<String> eventCodes) {
        Map<String, EntityTransferMasterLists> eventCodeMasterDataMap = new HashMap<>();
        log.info("EventService: received {} event codes for fetching Masterdata", eventCodes.size());
        if (CollectionUtils.isEmpty(eventCodes))
            return eventCodeMasterDataMap;
        try {
            List<Object> masterDataListCriteria = Arrays.asList(
                    List.of(
                            List.of(MasterDataConstants.ITEM_TYPE),
                            "=",
                            MasterDataType.ORDER_EVENTS.getId()
                    ),
                    "and",
                    List.of(
                            List.of(MasterDataConstants.ITEM_VALUE),
                            "IN",
                            List.of(eventCodes)
                    )
            );
            CommonV1ListRequest v1ListRequest = CommonV1ListRequest.builder().criteriaRequests(masterDataListCriteria).build();
            var v1DataResponse = iv1Service.fetchMasterData(v1ListRequest);
            List<EntityTransferMasterLists> masterData = jsonHelper.convertValueToList(v1DataResponse.getEntities(), EntityTransferMasterLists.class);
            masterData.forEach(i -> eventCodeMasterDataMap.put(i.getItemValue(), i));
        } catch (Exception e) {
            log.error("EventService : Error fetching masterdata for event codes", e);
        }

        return eventCodeMasterDataMap;
    }

    public boolean checkIfPartyExists(PartiesResponse party) {
        return !Objects.isNull(party) && !isStringNullOrEmpty(party.getOrgCode());
    }

    public boolean checkIfPartyExists(Parties party) {
        return !Objects.isNull(party) && !isStringNullOrEmpty(party.getOrgCode());
    }

    public String getCountryFromUnLocCode(String unLocCode) {
        if (isStringNullOrEmpty(unLocCode) || unLocCode.length() < 2)
            return null;
        return getAlpha3FromAlpha2(unLocCode.substring(0, 2));
    }

    public String getTwoDigitCountryFromUnLocCode(String unLocCode) {
        if (isStringNullOrEmpty(unLocCode) || unLocCode.length() < 2)
            return null;
        return unLocCode.substring(0, 2);
    }

    public void checkForMandatoryHsCodeForUAE(Awb awb) {
        String destinationPortLocCode = null;
        if (awb.getShipmentId() != null) {
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(awb.getShipmentId());
            if (shipmentDetails.isPresent())
                destinationPortLocCode = shipmentDetails.get().getCarrierDetails().getDestinationPortLocCode();
        } else if (awb.getConsolidationId() != null) {
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(awb.getConsolidationId());
            if (consolidationDetails.isPresent())
                destinationPortLocCode = consolidationDetails.get().getCarrierDetails().getDestinationPortLocCode();
        }

        if (destinationPortLocCode != null && destinationPortLocCode.startsWith(UAE_TWO_DIGIT_IATA_CODE)) {
            List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfoList = awb.getAwbGoodsDescriptionInfo();
            awbGoodsDescriptionInfoList.forEach(awbGoodsDescriptionInfo -> {
                if (Objects.isNull(awbGoodsDescriptionInfo.getHsCode())) {
                    throw new ValidationException("Please enter the HS code in the goods description of the cargo information tab.");
                }
            });
        }
    }

    public String getAutoPopulateDepartment(String transportMode, String direction, String module) {
        String department = null;
        List<Map<String, Object>> departmentList = mdmServiceAdapter.getDepartmentList(transportMode, direction, module);
        if (!CollectionUtils.isEmpty(departmentList)) {
            List<String> uniqueDepartments = departmentList.stream()
                    .map(i -> StringUtility.convertToString(i.get(MdmConstants.DEPARTMENT)))
                    .distinct().toList();
            department = uniqueDepartments.size() == 1 ? StringUtility.convertToString(uniqueDepartments.get(0)) : null;
        }
        return department;
    }

    public ShipmentDetailsResponse getShipmentDetailsResponse(ShipmentDetails shipmentDetails, List<String> includeColumns) {
        return setIncludedFields(shipmentDetails, includeColumns);
    }

    private ShipmentDetailsResponse setIncludedFields(ShipmentDetails shipmentDetail, List<String> includeColumns) {
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();

        includeColumns.forEach(field -> {
            try {
                String capitalizedField = capitalize(field);

                // Reflectively obtain the getter and setter methods once
                Method getter = ShipmentDetails.class.getMethod("get" + capitalizedField);
                Object value = getter.invoke(shipmentDetail);

                Object dtoValue = getDtoValue(value);
                Class<?> paramType;
                if (dtoValue instanceof List<?> list && !list.isEmpty()) {
                    paramType = List.class;
                } else if (dtoValue != null) {
                    paramType = dtoValue.getClass();
                } else {
                    paramType = getter != null ? getter.getReturnType() : Object.class;
                }

                Method setter = ShipmentDetailsResponse.class.getMethod("set" + capitalizedField, paramType);
                setter.invoke(shipmentDetailsResponse, dtoValue != null ? dtoValue : value);
            } catch (Exception e) {
                log.error("No such field: {}", field, e.getMessage());
            }
        });
        return shipmentDetailsResponse;
    }

    @Nullable
    public Object getDtoValue(Object value) {
        Object dtoValue = null;
        if (value instanceof CarrierDetails) {
            dtoValue = modelMapper.map(value, CarrierDetailResponse.class);
        } else if (value instanceof AdditionalDetails) {
            dtoValue = modelMapper.map(value, AdditionalDetailResponse.class);
        } else if (value instanceof PickupDeliveryDetails) {
            dtoValue = modelMapper.map(value, PickupDeliveryDetailsResponse.class);
        } else if (value instanceof Parties) {
            dtoValue = modelMapper.map(value, PartiesResponse.class);
        }

        if (value instanceof List<?>) {
            List<?> list = (List<?>) value;
            if (!list.isEmpty()) {
                Class<?> firstElementClass = list.get(0).getClass();
                Type targetType = getTypeTokenMap().get(firstElementClass);
                if (targetType != null) {
                    dtoValue = modelMapper.map(value, targetType);
                }
            }
        }
        return dtoValue;
    }

    private Map<Class<?>, Type> getTypeTokenMap() {
        Map<Class<?>, Type> typeTokenMap = new HashMap<>();
        typeTokenMap.put(Containers.class, new TypeToken<List<ContainerResponse>>() {
        }.getType());
        typeTokenMap.put(BookingCarriage.class, new TypeToken<List<BookingCarriageResponse>>() {
        }.getType());
        typeTokenMap.put(ELDetails.class, new TypeToken<List<ELDetailsResponse>>() {
        }.getType());
        typeTokenMap.put(Events.class, new TypeToken<List<EventsResponse>>() {
        }.getType());
        typeTokenMap.put(Packing.class, new TypeToken<List<PackingResponse>>() {
        }.getType());
        typeTokenMap.put(ReferenceNumbers.class, new TypeToken<List<ReferenceNumbersResponse>>() {
        }.getType());
        typeTokenMap.put(Routings.class, new TypeToken<List<RoutingsResponse>>() {
        }.getType());
        typeTokenMap.put(ServiceDetails.class, new TypeToken<List<ServiceDetailsResponse>>() {
        }.getType());
        typeTokenMap.put(TruckDriverDetails.class, new TypeToken<List<TruckDriverDetailsResponse>>() {
        }.getType());
        typeTokenMap.put(Notes.class, new TypeToken<List<NotesResponse>>() {
        }.getType());
        typeTokenMap.put(Jobs.class, new TypeToken<List<JobResponse>>() {
        }.getType());
        typeTokenMap.put(ConsolidationDetails.class, new TypeToken<List<ConsolidationListResponse>>() {
        }.getType());
        typeTokenMap.put(Parties.class, new TypeToken<List<PartiesResponse>>() {
        }.getType());
        typeTokenMap.put(ShipmentOrder.class, new TypeToken<List<ShipmentOrderResponse>>() {
        }.getType());
        typeTokenMap.put(CarrierDetails.class, new TypeToken<List<CarrierDetailResponse>>() {
        }.getType());
        return typeTokenMap;
    }


    private String capitalize(String str) {
        if (str == null || str.isEmpty()) return str;
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }

    public List<Long> getTriangulationPartnerList(List<TriangulationPartner> partnerList) {
        return partnerList != null ? partnerList.stream()
                .filter(Objects::nonNull)
                .map(TriangulationPartner::getTriangulationPartner)
                .toList() : Collections.emptyList();
    }

    public List<Long> getTenantIdsFromEntity(ListCousinBranchesForEtRequest request) {

        Set<Long> tenantIds = new HashSet<>();
        Long sourceTenantId = TenantContext.getCurrentTenant().longValue();
        Long receivingBranch = null;
        List<Long> triangulationPartners = null;
        List<Long> otherIds = new ArrayList<>();
        Optional<ShipmentDetails> optionalShipmentDetails;
        Optional<ConsolidationDetails> optionalConsolidationDetails;

        if (Objects.equals(request.getEntityType(), Constants.SHIPMENT) && (request.getEntityId() != null || request.getEntityGuid() != null)) {
            optionalShipmentDetails = getShipmentDetails(request);

            if (optionalShipmentDetails.isEmpty()) {
                return new ArrayList<>();
            }
            ShipmentDetails shipmentDetails = optionalShipmentDetails.get();
            sourceTenantId = Long.valueOf(shipmentDetails.getTenantId());

            // Add tenant data from all shipments sharing the same parent GUID
            addTenantDataFromParentGuid(shipmentDetails.getParentGuid(), tenantIds, Constants.SHIPMENT);

            if (Boolean.TRUE.equals(request.getIsReassign())) {
                receivingBranch = shipmentDetails.getReceivingBranch();
                triangulationPartners = extractTriangulationPartners(shipmentDetails.getTriangulationPartnerList());
            }

            processConsolidationDetails(shipmentDetails, request.getIsReceivingBranch(), request.getIsTriangulationBranch(), otherIds);
        } else if (Objects.equals(request.getEntityType(), Constants.CONSOLIDATION) && (request.getEntityId() != null || request.getEntityGuid() != null)) {
            optionalConsolidationDetails = getConsolidationDetails(request);
            if (optionalConsolidationDetails.isEmpty()) {
                return new ArrayList<>();
            }
            ConsolidationDetails consolidationDetails = optionalConsolidationDetails.get();
            sourceTenantId = Long.valueOf(consolidationDetails.getTenantId());

            // Add tenant IDs from all consolidation entities sharing the same parent GUID along with interbranch cases
            addTenantDataFromParentGuid(consolidationDetails.getParentGuid(), tenantIds, Constants.CONSOLIDATION);

            if (Boolean.TRUE.equals(request.getIsReassign())) {
                receivingBranch = consolidationDetails.getReceivingBranch();
                triangulationPartners = extractTriangulationPartners(consolidationDetails.getTriangulationPartnerList());
            }

            processConsolidationShipments(consolidationDetails, request.getIsReceivingBranch(), request.getIsTriangulationBranch(), otherIds);
        }

        tenantIds.add(sourceTenantId);
        if (receivingBranch != null) {
            tenantIds.add(receivingBranch);
        }
        addAllIfNotEmpty(tenantIds, triangulationPartners);
        addAllIfNotEmpty(tenantIds, otherIds);
        return tenantIds.stream().filter(Objects::nonNull).toList();
    }

    public void addTenantDataFromParentGuid(UUID parentGuid, Set<Long> tenantIds, String entity) {
        if (parentGuid == null) {
            return;
        }
        if (Objects.equals(entity, Constants.CONSOLIDATION)) {

            Optional<ConsolidationDetails> optionalConsolidationDetails = consolidationDetailsDao.findConsolidationByGuidWithQuery(parentGuid);
            if(optionalConsolidationDetails.isEmpty())
                return;

            ConsolidationDetails parentConsolidationDetails = optionalConsolidationDetails.get(); // parentConsolidation
            handleParentEntity(tenantIds, parentConsolidationDetails.getTenantId(), parentConsolidationDetails.getSourceTenantId(), parentConsolidationDetails.getReceivingBranch(), parentConsolidationDetails.getTriangulationPartnerList());
            handleInterbranchConsolidation(tenantIds, parentConsolidationDetails);

            List<ConsolidationDetails> relatedConsolidations = consolidationDetailsDao.findByParentGuid(parentGuid);
            if (!CollectionUtils.isEmpty(relatedConsolidations)) {
                relatedConsolidations.forEach(c -> addTenantIdAndTriangulationData(tenantIds, c.getTenantId(), c.getTriangulationPartnerList()));
            }

        } else if (Objects.equals(entity, Constants.SHIPMENT)) {

            Optional<ShipmentDetails> optionalShipmentDetails = shipmentDao.findShipmentByGuidWithQuery(parentGuid);
            if(optionalShipmentDetails.isEmpty())
                return;

            ShipmentDetails parentShipmentDetails = optionalShipmentDetails.get(); // parent Shipment
            handleParentEntity(tenantIds, parentShipmentDetails.getTenantId(), parentShipmentDetails.getSourceTenantId(), parentShipmentDetails.getReceivingBranch(), parentShipmentDetails.getTriangulationPartnerList());

            List<ShipmentDetails> relatedShipments = shipmentDao.findByParentGuid(parentGuid);
            if (!CollectionUtils.isEmpty(relatedShipments)) {
                relatedShipments.forEach(s -> addTenantIdAndTriangulationData(tenantIds, s.getTenantId(), s.getTriangulationPartnerList()));
            }
        }
    }

    public void addTenantIdAndTriangulationData(Set<Long> tenantIds, Integer tenantId, List<TriangulationPartner> triangulationPartnerList) {
        if (tenantId != null) {
            tenantIds.add(Long.valueOf(tenantId));
        }
        if (!CollectionUtils.isEmpty(triangulationPartnerList)) {
            List<Long> triangulationPartners = extractTriangulationPartners(triangulationPartnerList);
            addAllIfNotEmpty(tenantIds, triangulationPartners);
        }
    }

    public void handleInterbranchConsolidation(Set<Long> tenantIds, ConsolidationDetails parentConsolidationDetails) {
        if(Boolean.TRUE.equals(parentConsolidationDetails.getInterBranchConsole())) {
            parentConsolidationDetails.getShipmentsList().forEach(s -> addTenantIdAndTriangulationData(tenantIds, s.getTenantId(), s.getTriangulationPartnerList()));
        }
    }

    public void handleParentEntity(Set<Long> tenantIds, Integer tenantId, Long receivingBranch, Long originBranch, List<TriangulationPartner> triangulationPartnerList) {
        if (receivingBranch != null) {
            tenantIds.add(receivingBranch);
        }
        if (originBranch != null) {
            tenantIds.add(originBranch);
        }
        addTenantIdAndTriangulationData(tenantIds, tenantId, triangulationPartnerList);
    }

    private Optional<ShipmentDetails> getShipmentDetails(ListCousinBranchesForEtRequest request) {
        Optional<ShipmentDetails> optionalShipmentDetails;
        if (request.getEntityId() != null) {
            optionalShipmentDetails = shipmentDao.findShipmentByIdWithQuery(request.getEntityId());
        } else {
            optionalShipmentDetails = shipmentDao.findShipmentByGuidWithQuery(UUID.fromString(request.getEntityGuid()));
        }
        return optionalShipmentDetails;
    }

    private Optional<ConsolidationDetails> getConsolidationDetails(ListCousinBranchesForEtRequest request) {
        Optional<ConsolidationDetails> optionalConsolidationDetails;
        if (request.getEntityId() != null) {
            optionalConsolidationDetails = consolidationDetailsDao.findConsolidationByIdWithQuery(request.getEntityId());
        } else {
            optionalConsolidationDetails = consolidationDetailsDao.findConsolidationByGuidWithQuery(UUID.fromString(request.getEntityGuid()));
        }
        return optionalConsolidationDetails;
    }

    private void addAllIfNotEmpty(Set<Long> set, List<Long> list) {
        if (list != null && !list.isEmpty()) {
            set.addAll(list);
        }
    }

    private void processConsolidationDetails(ShipmentDetails shipmentDetails, Boolean isReceivingBranch, Boolean isTriangulationBranch, List<Long> otherIds) {
        Set<ConsolidationDetails> consolidationList = shipmentDetails.getConsolidationList();
        if (consolidationList != null && !consolidationList.isEmpty()) {
            ConsolidationDetails consolidationDetails = consolidationList.iterator().next();
            if (Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole())) {
                if (Boolean.TRUE.equals(isReceivingBranch)) {
                    otherIds.add(Long.valueOf(consolidationDetails.getTenantId()));
                    otherIds.addAll(extractTriangulationPartners(consolidationDetails.getTriangulationPartnerList()));
                    consolidationDetails.getShipmentsList().forEach(s -> otherIds.add(Long.valueOf(s.getTenantId())));
                }
                if (Boolean.TRUE.equals(isTriangulationBranch)) {
                    otherIds.add(consolidationDetails.getReceivingBranch());
                    otherIds.addAll(extractTriangulationPartners(consolidationDetails.getTriangulationPartnerList()));
                    consolidationDetails.getShipmentsList().forEach(s -> {
                        otherIds.add(Long.valueOf(s.getTenantId()));
                        if (!Objects.equals(s.getId(), shipmentDetails.getId())) {
                            otherIds.add(s.getReceivingBranch());
                        }
                    });
                }
            }
        }
    }

    private void processConsolidationShipments(ConsolidationDetails consolidationDetails, Boolean isReceivingBranch, Boolean isTriangulationBranch, List<Long> otherIds) {
        if (Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole())) {
            if (Boolean.TRUE.equals(isReceivingBranch)) {
                consolidationDetails.getShipmentsList().forEach(s -> otherIds.add(Long.valueOf(s.getTenantId())));
            }
            if (Boolean.TRUE.equals(isTriangulationBranch)) {
                consolidationDetails.getShipmentsList().forEach(s -> {
                    otherIds.add(s.getReceivingBranch());
                    otherIds.add(Long.valueOf(s.getTenantId()));
                });
            }
        }
    }

    private List<Long> extractTriangulationPartners(List<TriangulationPartner> partners) {
        return Optional.ofNullable(partners)
                .orElse(Collections.emptyList())
                .stream()
                .map(TriangulationPartner::getTriangulationPartner)
                .toList();
    }

    public Parties removeIdFromParty(Parties parties) {
        if (parties == null || isStringNullOrEmpty(parties.getOrgId()))
            return null;
        PartiesRequest partiesRequest = jsonHelper.convertValue(parties, PartiesRequest.class);
        partiesRequest.setId(null);
        return jsonHelper.convertValue(partiesRequest, Parties.class);
    }

    public static boolean checkSameParties(Parties obj1, Parties obj2) {
        if (obj1 == null && obj2 == null) return true;
        if (obj1 == null || obj2 == null) return false;

        return Objects.equals(obj1.getOrgId(), obj2.getOrgId()) &&
                Objects.equals(obj1.getAddressId(), obj2.getAddressId());
    }

    public static boolean checkPartyNotNull(Parties party) {
        if (party == null) return false;
        else if (party.getOrgId() == null) return false;
        else return !isStringNullOrEmpty(party.getOrgId());
    }

    public static boolean checkAddressNotNull(Parties party) {
        if (party == null) return false;
        else if (isStringNullOrEmpty(party.getOrgId())) return false;
        else return !isStringNullOrEmpty(party.getAddressId());
    }

    public static boolean checkAddressNotNull(PartiesResponse party) {
        if (party == null) return false;
        else if (isStringNullOrEmpty(party.getOrgId())) return false;
        else return !isStringNullOrEmpty(party.getAddressId());
    }

    public Long getReceivingBranch(String orgIdString, String addressIdString) {
        Long orgId = Long.valueOf(orgIdString);
        Long addressId = Long.valueOf(addressIdString);
        TenantFilterRequest request = TenantFilterRequest.builder().orgId(orgId).addressId(addressId).build();
        V1DataResponse response = iv1Service.listBranchesByDefaultOrgAndAddress(request);
        if (Objects.nonNull(response.getEntities())) {
            List<V1TenantResponse> tenantResponses = jsonHelper.convertValueToList(response.getEntities(), V1TenantResponse.class);
            if (!tenantResponses.isEmpty())
                return tenantResponses.get(0).getTenantId();
        }
        return null;
    }

    public static boolean checkAirSecurityForShipment(ShipmentDetails shipmentDetails) {
        if (null != shipmentDetails.getTransportMode()
                && null != shipmentDetails.getDirection()
                && shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)
                && shipmentDetails.getDirection().equals(DIRECTION_EXP)) {
            return UserContext.isAirSecurityUser();
        }
        return true;
    }

    public static boolean checkAirSecurityForConsolidation(ConsolidationDetails consolidationDetails) {
        if (consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && consolidationDetails.getShipmentType().equals(DIRECTION_EXP)) {
            return UserContext.isAirSecurityUser();
        }
        return true;
    }

    public static boolean checkAirSecurityForBooking(CustomerBooking customerBooking) {
        if (customerBooking.getTransportType().equals(Constants.TRANSPORT_MODE_AIR) && DIRECTION_EXP.equals(customerBooking.getDirection())) {
            return UserContext.isAirSecurityUser();
        }
        return true;
    }

    public static boolean checkAirSecurityForBookingRequest(CustomerBookingRequest customerBooking) {
        if (customerBooking.getTransportType().equals(Constants.TRANSPORT_MODE_AIR) && customerBooking.getDirection().equals(DIRECTION_EXP)) {
            return UserContext.isAirSecurityUser();
        }
        return true;
    }

    public void validateAirSecurityAndDGShipmentPermissions(ShipmentDetails shipmentDetails) {
        if(Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(shipmentDetails.getTransportMode())) {
            // Air EXP shipment can be updated only by Air Security users
            if(Boolean.TRUE.equals(getShipmentSettingFromContext().getCountryAirCargoSecurity()) &&
                    !CommonUtils.checkAirSecurityForShipment(shipmentDetails)) {
                throw new ValidationException("You don't have Air Security permission to create or update AIR EXP Shipment.");
            }
        }
    }

    public void validateAirSecurityAndDGConsolidationPermissions(ConsolidationDetails consolidationDetails) {
        if(Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(consolidationDetails.getTransportMode())) {
            // Air EXP shipment can be updated only by Air Security users
            if(Boolean.TRUE.equals(getShipmentSettingFromContext().getCountryAirCargoSecurity()) &&
                    !CommonUtils.checkAirSecurityForConsolidation(consolidationDetails)) {
                throw new ValidationException("You don't have Air Security permission to create or update AIR EXP Consolidation.");
            }
        }
    }

    public EventsRequest prepareEventRequest(Long entityId, String eventCode, String entityType, String referenceNumber) {
        EventsRequest eventsRequest = new EventsRequest();
        eventsRequest.setActual(getUserZoneTime(LocalDateTime.now()));
        eventsRequest.setEntityId(entityId);
        eventsRequest.setEntityType(entityType);
        eventsRequest.setEventCode(eventCode);
        if (!CommonUtils.isStringNullOrEmpty(referenceNumber))
            eventsRequest.setContainerNumber(referenceNumber);
        eventsRequest.setIsPublicTrackingEvent(true);
        eventsRequest.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);
        return eventsRequest;
    }

    public LocalDateTime getUserZoneTime(LocalDateTime inputDateTime) {
        UsersDto userDetails = UserContext.getUser();
        Boolean enableTimeZoneFlag = Optional.ofNullable(userDetails.getEnableTimeZone()).orElse(false);
        String tenantTimeZone = userDetails.getTimeZoneId();
        return DateUtils.convertDateToUserTimeZone(inputDateTime, null, tenantTimeZone, enableTimeZoneFlag);
    }

    public void impersonateUser(Integer tenantId) {
        try {
            Map<Integer, Object> responseMap = getTenantSettingsAndTenantsData(Set.of(tenantId));
            TenantDetailsByListResponse.TenantDetails response = modelMapper.map(responseMap.get(tenantId), TenantDetailsByListResponse.TenantDetails.class);
            TenantModel tenantModel = modelMapper.map(response.getTenant(), TenantModel.class);
            UserContext.getUser().setEnableTimeZone(tenantModel.getEnableTimeZone());
            UserContext.getUser().setTimeZoneId(tenantModel.getTimeZoneId());
            log.info("User impersonated successfully");
        } catch (Exception e) {
            // in case of any failure set Enable Time zone and time zone id as null to avoid wrong transformations
            // UTC will be the fallback value
            UserContext.getUser().setEnableTimeZone(null);
            UserContext.getUser().setTimeZoneId(null);
            log.warn("Error while impersonating user with tenant Id {}", tenantId, e);
        }
    }
    public Object setIncludedFieldsToResponse(Object entity, Set<String> includeColumns, Object response) {
        FieldClassifier classifier = classifyFields(includeColumns);

        processRegularFields(entity, classifier.regularFields, response);
        processCollectionFields(entity, classifier.collectionFields, response);

        return response;
    }

    private FieldClassifier classifyFields(Set<String> includeColumns) {
        FieldClassifier classifier = new FieldClassifier();

        for (String field : includeColumns) {
            if(field==null || field.trim().isEmpty()) continue;
            if (isCollectionFieldRequiringPartialMapping(field, includeColumns)) {
                classifier.addCollectionField(field);
            } else {
                classifier.addRegularField(field);
            }
        }

        return classifier;
    }

    private boolean isCollectionFieldRequiringPartialMapping(String field, Set<String> allFields) {
        return isCollectionFieldWithSubFields(field, allFields)
                && !field.contains("orgData")
                && !field.contains("addressData");
    }

    private void processRegularFields(Object entity, Set<String> regularFields, Object response) {
        regularFields.forEach(field -> processRegularField(entity, field, response));
    }

    private void processRegularField(Object entity, String field, Object response) {
        try {
            Object value = getNestedFieldValue(entity, field);
            if (value == null) return;

            Object dtoValue = mapToDTO(value, response, field);
            setNestedFieldValue(response, field, dtoValue != null ? dtoValue : value);
        } catch (Exception e) {
            log.error("Failed to process regular field: {}", field, e);
        }
    }

    private void processCollectionFields(Object entity, Map<String, Set<String>> collectionFields, Object response) {
        collectionFields.forEach((rootField, subFields) ->
                processCollectionField(entity, rootField, subFields, response));
    }

    private void processCollectionField(Object entity, String rootField, Set<String> subFields, Object response) {
        try {
            Object value = getNestedFieldValue(entity, rootField);
            if (value == null) return;

            Object dtoValue = createCollectionDTO(value, rootField, subFields);
            setNestedFieldValue(response, rootField, dtoValue != null ? dtoValue : value);
        } catch (Exception e) {
            log.error("Failed to process collection field: {}", rootField, e);
        }
    }

    private Object createCollectionDTO(Object value, String rootField, Set<String> subFields) {
        Set<String> subFieldNames = getSubFieldsForRoot(rootField, subFields);

        if (value instanceof List<?>) {
            return mapListWithSelectedFields((List<?>) value, subFieldNames);
        } else if (value instanceof Set<?>) {
            return mapSetWithSelectedFields((Set<?>) value, subFieldNames);
        } else {
            return mapToDTO(value);
        }
    }

    // Helper class to organize field classification
    private static class FieldClassifier {
        final Set<String> regularFields = new HashSet<>();
        final Map<String, Set<String>> collectionFields = new HashMap<>();

        void addRegularField(String field) {
            regularFields.add(field);
        }

        void addCollectionField(String field) {
            String rootField = field.split("\\.")[0];
            collectionFields.computeIfAbsent(rootField, k -> new HashSet<>()).add(field);
        }
    }
    // Helper method to identify if a field is a collection field that needs partial mapping
    private boolean isCollectionFieldWithSubFields(String field, Set<String> includeColumns) {
        if (!field.contains(".")) {
            return false; // Not a nested field
        }

        String rootField = field.split("\\.")[0];

        // Check if this root field appears multiple times with different sub-fields
        // This indicates it's likely a collection that needs partial mapping
        long subFieldCount = includeColumns.stream()
                .filter(f -> f.startsWith(rootField + "."))
                .count();

        // If there are multiple sub-fields for the same root, it's likely a collection
        // Also check if the root field alone is NOT in the includeColumns (indicating partial selection)
        return subFieldCount > 0 && !includeColumns.contains(rootField);
    }

    // Helper method to get sub-fields for a root field
    private Set<String> getSubFieldsForRoot(String rootField, Set<String> includeColumns) {
        return includeColumns.stream()
                .filter(field -> field.startsWith(rootField + "."))
                .map(field -> field.substring(rootField.length() + 1))
                .collect(Collectors.toSet());
    }

    // Method to map list with only selected fields
    private Object mapListWithSelectedFields(List<?> list, Set<String> selectedFields) {
        if (list.isEmpty()) return list;

        List<Object> mappedList = new ArrayList<>();
        for (Object item : list) {
            Object mappedItem = mapToDTO(item); // First convert to DTO
            if (mappedItem != null) {
                Object partialItem = createPartialObject(mappedItem, selectedFields);
                mappedList.add(partialItem);
            }
        }
        return mappedList;
    }

    // Method to map set with only selected fields
    private Object mapSetWithSelectedFields(Set<?> set, Set<String> selectedFields) {
        if (set.isEmpty()) return set;

        Set<Object> mappedSet = new HashSet<>();
        for (Object item : set) {
            Object mappedItem = mapToDTO(item); // First convert to DTO
            if (mappedItem != null) {
                Object partialItem = createPartialObject(mappedItem, selectedFields);
                mappedSet.add(partialItem);
            }
        }
        return mappedSet;
    }

    // Method to create partial object with only selected fields
    private Object createPartialObject(Object source, Set<String> selectedFields) {
        try {
            // Create a Map to hold only the selected fields
            Map<String, Object> partialObject = new HashMap<>();

            setPartialObject(source, selectedFields, partialObject);

            return partialObject;
        } catch (Exception e) {
            log.error("Error creating partial object: {}", e.getMessage());
            return source; // Return original object if partial creation fails
        }
    }

    private void setPartialObject(Object source, Set<String> selectedFields, Map<String, Object> partialObject) {
        for (String field : selectedFields) {
            try {
                Object fieldValue = getNestedFieldValue(source, field);
                if (fieldValue != null) {
                    partialObject.put(field, fieldValue);
                }
            } catch (Exception e) {
                log.debug("Could not extract field {} from object: {}", field, e.getMessage());
            }
        }
    }

    public Object mapToDTO(Object value) {
        if (value instanceof CarrierDetails) {
            return modelMapper.map(value, CarrierDetailResponse.class);
        } else if (value instanceof AdditionalDetails) {
            return modelMapper.map(value, AdditionalDetailResponse.class);
        } else if (value instanceof PickupDeliveryDetails) {
            return modelMapper.map(value, PickupDeliveryDetailsResponse.class);
        } else if (value instanceof Parties) {
            return modelMapper.map(value, PartiesResponse.class);
        } else if (value instanceof ArrivalDepartureDetails) {
            return modelMapper.map(value, ArrivalDepartureDetailsResponse.class);
        } else if (value instanceof Packing) {
            // Handle single Packing object
            return modelMapper.map(value, PackingResponse.class);
        } else if (value instanceof SailingInformation) {
            return modelMapper.map(value, SailingInformationResponse.class);
        } else if (value instanceof List<?>) {
            return mapListToDTO(value);
        } else if (value instanceof Set) {
            return mapListToDTOSet(value);
        }
        return value; // Return as is if not mappable
    }

    // New overloaded method that considers the target response type
    public Object mapToDTO(Object value, Object response, String fieldPath) {
        if (value instanceof CarrierDetails) {
            return modelMapper.map(value, CarrierDetailResponse.class);
        } else if (value instanceof AdditionalDetails) {
            // Check what type the response expects for this field
            Class<?> expectedType = getExpectedFieldType(response, fieldPath);
            if (expectedType != null && AdditionalDetailsListResponse.class.isAssignableFrom(expectedType)) {
                return modelMapper.map(value, AdditionalDetailsListResponse.class);
            } else {
                return modelMapper.map(value, AdditionalDetailResponse.class);
            }
        } else if (value instanceof PickupDeliveryDetails) {
            return modelMapper.map(value, PickupDeliveryDetailsResponse.class);
        } else if (value instanceof Parties) {
            return modelMapper.map(value, PartiesResponse.class);
        } else if (value instanceof ArrivalDepartureDetails) {
            return modelMapper.map(value, ArrivalDepartureDetailsResponse.class);
        } else if (value instanceof Packing) {
            return modelMapper.map(value, PackingResponse.class);
        } else if (value instanceof List<?>) {
            return mapListToDTO(value);
        } else if (value instanceof Set) {
            return mapListToDTOSet(value);
        }
        return value; // Return as is if not mappable
    }

    // Helper method to get the expected field type from the response object
    private Class<?> getExpectedFieldType(Object response, String fieldPath) {
        try {
            String[] fields = fieldPath.split("\\.");
            Class<?> currentClass = response.getClass();

            for (int i = 0; i < fields.length - 1; i++) {
                Method getter = currentClass.getMethod("get" + capitalizeV3(fields[i]));
                currentClass = getter.getReturnType();
                if (currentClass == null) {
                    return null;
                }
            }

            String lastField = fields[fields.length - 1];
            Method setter = findSetterMethod(currentClass, lastField);
            if (setter != null && setter.getParameterCount() > 0) {
                return setter.getParameterTypes()[0];
            }
        } catch (Exception e) {
            log.debug("Could not determine expected field type for path: {}", fieldPath);
        }
        return null;
    }

    // Helper method to find setter method
    private Method findSetterMethod(Class<?> clazz, String fieldName) {
        String setterName = "set" + capitalizeV3(fieldName);
        for (Method method : clazz.getMethods()) {
            if (method.getName().equals(setterName) && method.getParameterCount() == 1) {
                return method;
            }
        }
        return null;
    }

    public Object mapListToDTOSet(Object value) {
        Set<?> set = (Set<?>) value;
        if (set.isEmpty()) return value;
        Object obj = set.iterator().next();
        if (obj instanceof Containers) {
            return modelMapper.map(value, new TypeToken<Set<ContainerResponse>>() {
            }.getType());
        } else if (obj instanceof ConsolidationDetails) {
            return modelMapper.map(value, new TypeToken<Set<ConsolidationListResponse>>() {
            }.getType());
        } else if (obj instanceof Packing) {
            // Handle Set of Packing objects
            return modelMapper.map(value, new TypeToken<Set<PackingResponse>>() {
            }.getType());
        }
        return value;
    }

    public Object mapListToDTO(Object value) {
        List<?> list = (List<?>) value;
        if (list.isEmpty()) return value;

        Type targetType = DTO_TYPE_MAP.get(list.get(0).getClass());
        if (targetType != null) {
            return modelMapper.map(value, targetType);
        }

        return checkForTriangulationPartnerList(value, list);
    }

    private Object checkForTriangulationPartnerList(Object value, List<?> list) {
        if (!list.isEmpty() && list.get(0) instanceof TriangulationPartner) {
            return modelMapper.map(value, new TypeToken<List<TriangulationPartnerResponse>>() {
            }.getType());
        }
        return value;
    }

    public Object getNestedFieldValue(Object object, String fieldPath) throws NoSuchMethodException, RunnerException {
        String[] fields = fieldPath.split("\\.");
        Object currentValue = object;

        for (int i = 0; i < fields.length; i++) {
            currentValue = getFieldValue(currentValue, fields[i], fieldPath, i == fields.length - 1);
            if (currentValue == null) {
                return null;
            }
        }
        return currentValue;
    }

    private Object getFieldValue(Object value, String field, String fullPath, boolean isLastField) throws NoSuchMethodException, RunnerException {
        if (value == null) {
            log.debug("Null value encountered at field: {} in path: {}", field, fullPath);
            return null;
        }

        try {
            if (value instanceof List<?>) {
                return handleListAccess((List<?>) value, field, fullPath, isLastField);
            } else if (value instanceof Map) {
                return handleMapAccess((Map<?, ?>) value, field);
            } else {
                return handleObjectAccess(value, field);
            }
        } catch (IllegalAccessException | InvocationTargetException e) {
            log.error("Error accessing field: {} in path: {}", field, fullPath, e);
            throw new RunnerException(String.format("Error accessing field:  %s. Error: %s",
                    field, e.getMessage()));
        }
    }

    private Object handleListAccess(List<?> list, String field, String fullPath, boolean isLastField) {
        if (isNumeric(field)) {
            return getListElementByIndex(list, field, fullPath);
        } else if (isLastField) {
            return list; // Return entire list for final field
        } else {
            log.debug("Unexpected nested access on List at field: {} in path: {}", field, fullPath);
            return null;
        }
    }

    private Object getListElementByIndex(List<?> list, String field, String fullPath) {
        int index = Integer.parseInt(field);
        if (index >= 0 && index < list.size()) {
            return list.get(index);
        } else {
            log.debug("Index {} out of bounds for list of size {} in path: {}", index, list.size(), fullPath);
            return null;
        }
    }

    private Object handleMapAccess(Map<?, ?> map, String field) {
        return map.get(field);
    }

    private Object handleObjectAccess(Object value, String field) throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        Method getter = findGetter(value.getClass(), field);
        if (getter != null) {
            return getter.invoke(value);
        } else {
            throw new NoSuchMethodException("No getter found for field: " + field + " in " + value.getClass().getSimpleName());
        }
    }
    // Helper method to find getter with multiple naming conventions
    private Method findGetter(Class<?> clazz, String fieldName) {
        String capitalizedField = capitalizeV3(fieldName);

        // Try standard getter patterns
        String[] getterNames = {
                "get" + capitalizedField,
                "is" + capitalizedField,
                fieldName // Direct field name for some cases
        };

        for (String getterName : getterNames) {
            try {
                return clazz.getMethod(getterName);
            } catch (NoSuchMethodException e) {
                // Continue trying other patterns
            }
        }
        return null;
    }

    // Helper method to check if string is numeric
    private boolean isNumeric(String str) {
        try {
            Integer.parseInt(str);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    // Enhanced setNestedFieldValue with better error handling
    public void setNestedFieldValue(Object object, String fieldPath, Object value) throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, InstantiationException {
        String[] fields = fieldPath.split("\\.");
        Object target = object;

        for (int i = 0; i < fields.length - 1; i++) {
            String field = fields[i];
            Method getter;
            try {
                getter = target.getClass().getMethod("get" + capitalizeV3(field));
            } catch (NoSuchMethodException e) {
                // If no getter exists, assume it's a Map field
                if (target instanceof Map) {
                    Map<String, Object> mapTarget = (Map<String, Object>) target;
                    mapTarget.putIfAbsent(field, new HashMap<>());
                    target = mapTarget.get(field);
                    continue;
                } else {
                    throw e; // Rethrow exception if it's not a map
                }
            }
            Object nextTarget = getter.invoke(target);

            if (nextTarget == null) {
                Method setter = target.getClass().getMethod("set" + capitalizeV3(field), getter.getReturnType());
                if (Map.class.isAssignableFrom(getter.getReturnType())) {
                    nextTarget = new HashMap<>(); // Initialize Map
                } else if (List.class.isAssignableFrom(getter.getReturnType())) {
                    nextTarget = new ArrayList<>(); // Initialize List
                } else if (Set.class.isAssignableFrom(getter.getReturnType())) {
                    nextTarget = new HashSet<>(); // Initialize Set
                } else {
                    nextTarget = getter.getReturnType().getDeclaredConstructor().newInstance();
                }
                setter.invoke(target, nextTarget);
            }
            target = nextTarget;
        }

        String lastField = fields[fields.length - 1];
        setTargetValue(value, target, lastField);
    }

    public void setTargetValue(Object value, Object target, String lastField) throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
        if (target instanceof Map) {
            ((Map<String, Object>) target).put(lastField, value);
        } else {
            Method setter = findSetter(target.getClass(), lastField, value);
            if (setter != null) {
                setter.invoke(target, value);
            } else {
                throw new NoSuchMethodException("No setter found for field: " + lastField + " in " + target.getClass().getSimpleName());
            }
        }
    }

    // Enhanced setter finder
    private Method findSetter(Class<?> clazz, String fieldName, Object value) {
        String capitalizedField = capitalizeV3(fieldName);
        String setterName = "set" + capitalizedField;

        // Try different parameter types
        Class<?>[] paramTypes = {
                value != null ? value.getClass() : Object.class,
                Object.class,
                Map.class,
                List.class,
                Set.class
        };

        for (Class<?> paramType : paramTypes) {
            try {
                return clazz.getMethod(setterName, paramType);
            } catch (NoSuchMethodException e) {
                // Continue trying other parameter types
            }
        }

        // Try to find any setter with the right name
        for (Method method : clazz.getMethods()) {
            if (method.getName().equals(setterName) && method.getParameterCount() == 1) {
                Class<?> paramType = method.getParameterTypes()[0];
                if (value == null || paramType.isAssignableFrom(value.getClass())) {
                    return method;
                }
            }
        }

        return null;
    }

    /**
     * Capitalizes the first letter of a string.
     */
    public String capitalizeV3(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }

    public static List<String> splitAndTrimStrings(String input) {
        if (input == null || input.isEmpty()) {
            return List.of();
        }

        return Arrays.stream(input.split(","))
                .map(String::trim)
                .toList();
    }

    public static void includeRequiredColumns(Set<String> includeColumns) {
        includeColumns.add("id");
        includeColumns.add("guid");
    }
    public static BigDecimal roundBigDecimal(BigDecimal number, int decimalPlaces, RoundingMode roundingMode) {
        return number.setScale(decimalPlaces, roundingMode);
    }

    public static BigDecimal divide(BigDecimal consolidatedValue, BigDecimal value, Integer decimalPlaces, RoundingMode roundingMode) {
        if (value.compareTo(BigDecimal.ZERO) == 0) {
            return BigDecimal.ZERO; // Handle division by zero
        }
        return consolidatedValue.divide(value, decimalPlaces, roundingMode);
    }

    public static double calculatePercentage(BigDecimal consolidatedValue, BigDecimal value, Integer decimalPlaces, RoundingMode roundingMode) {
        BigDecimal percentage = divide(consolidatedValue, value, decimalPlaces, roundingMode).multiply(BigDecimal.valueOf(100));
        if (value.compareTo(BigDecimal.ZERO) == 0) {
            return 0.0;
        }
        return percentage.doubleValue();
    }

    public void sendEmailNotification(EmailTemplatesRequest emailTemplateModel, List<String> to, List<String> cc) {
        if(!to.isEmpty()) {
            try {
                notificationService.sendEmail(emailTemplateModel.getBody(),
                        emailTemplateModel.getSubject(), to, cc);
            } catch (Exception ex) {
                log.error(ex.getMessage());
            }
        }
    }
    public void sendExcelFileViaEmail(Workbook workbook, String filenameWithTimestamp) {
        try {
            SendEmailBaseRequest request = new SendEmailBaseRequest();
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            workbook.write(out);
            out.close();
            MultipartFile multipartFile = new WorkbookMultipartFile(workbook, filenameWithTimestamp);
            request.setTo(UserContext.getUser().getEmail());
            request.setSubject("Export Excel");
            request.setFile(multipartFile);
            notificationService.sendEmail(request);
            log.info("Email sent with Excel attachment");
        } catch (Exception e) {
            log.error("Error sending email: " + e.getMessage());
        }
    }

    public boolean isRoadLCLorLTL(String transportMode, String cargoType) {
        return Constants.TRANSPORT_MODE_ROA.equals(transportMode) && isLCLorLTL(cargoType);
    }

    public boolean isSeaLCL(String transportMode, String cargoType) {
        return Constants.TRANSPORT_MODE_SEA.equals(transportMode) && Constants.SHIPMENT_TYPE_LCL.equals(cargoType);
    }

    public boolean isLCLorLTL(String cargoType) {
        return (Constants.SHIPMENT_TYPE_LCL.equals(cargoType) || CARGO_TYPE_LTL.equals(cargoType));
    }

    public boolean isSeaFCLOrRoadFTL(String transportMode, String cargoType) {
        return isSeaFCL(transportMode, cargoType) || isRoadFTLOrRailFCL(transportMode, cargoType);
    }

    public boolean isRoadFTLOrRailFCL(String transportMode, String cargoType) {
        return (Constants.TRANSPORT_MODE_ROA.equals(transportMode) || TRANSPORT_MODE_RAI.equals(transportMode)) && isFCLorFTL(cargoType);
    }

    public boolean isSeaFCL(String transportMode, String cargoType) {
        return (Constants.TRANSPORT_MODE_SEA.equals(transportMode) || TRANSPORT_MODE_RAI.equals(transportMode)) && CARGO_TYPE_FCL.equals(cargoType);
    }

    public boolean isFCLorFTL(String cargoType) {
        return (CARGO_TYPE_FCL.equals(cargoType) || CARGO_TYPE_FTL.equals(cargoType));
    }
    public boolean isFCL(String cargoType) {
        return CARGO_TYPE_FCL.equals(cargoType);
    }
    public boolean isLCL(String cargoType) {
        return CARGO_TYPE_LCL.equals(cargoType);
    }

    public String getPacksUnit(String curUnit, String entityPacksUnit) {
        if(isStringNullOrEmpty(curUnit))
            curUnit = entityPacksUnit;
        else if(!isStringNullOrEmpty(entityPacksUnit) && !Objects.equals(curUnit, entityPacksUnit))
            curUnit = PackingConstants.PKG;
        return curUnit;
    }

    public String getPacksUnit(String curUnit) {
        if(isStringNullOrEmpty(curUnit))
            return PackingConstants.PKG;
        return curUnit;
    }

    public String getDefaultWeightUnit() {
        if(isStringNullOrEmpty(getShipmentSettingFromContext().getWeightChargeableUnit()))
            return Constants.WEIGHT_UNIT_KG;
        return getShipmentSettingFromContext().getWeightChargeableUnit();
    }

    public String getDefaultVolumeUnit() {
        if(isStringNullOrEmpty(getShipmentSettingFromContext().getVolumeChargeableUnit()))
            return VOLUME_UNIT_M3;
        return getShipmentSettingFromContext().getVolumeChargeableUnit();
    }
    public static String setTransportInfoStatusMessage(CarrierDetails carrierDetails, TransportInfoStatus transportInfoStatus, List<Routings> mainCarriageRoutings) {
        if (TransportInfoStatus.IH.equals(transportInfoStatus) && !CommonUtils.listIsNullOrEmpty(mainCarriageRoutings)) {
            Routings firstLeg = mainCarriageRoutings.get(0);
            Routings lastLeg = mainCarriageRoutings.get(mainCarriageRoutings.size() - 1);
            String polMessage = EMPTY_STRING;
            String podMessage = EMPTY_STRING;
            if (Objects.nonNull(carrierDetails) && !Objects.equals(firstLeg.getPol(), carrierDetails.getOriginPort())) {
                polMessage = Constants.POL_WARNING_MESSAGE;
            }
            if (Objects.nonNull(carrierDetails) && !Objects.equals(lastLeg.getPod(), carrierDetails.getDestinationPort())) {
                podMessage = Constants.POD_WARNING_MESSAGE;
            }
            if (StringUtility.isNotEmpty(polMessage) && StringUtility.isNotEmpty(podMessage)) {
                return Constants.POL_POD_WARNING_MESSAGE;
            } else if (StringUtility.isNotEmpty(polMessage)) {
                return Constants.POL_WARNING_MESSAGE;
            } else if (StringUtility.isNotEmpty(podMessage)) {
                return Constants.POD_WARNING_MESSAGE;
            }
        }
        return EMPTY_STRING;
    }
    public static Boolean isVesselVoyageOrCarrierFlightNumberAvailable(List<Routings> mainCarriageRoutings) {
        Optional<Routings> routings = mainCarriageRoutings.stream().filter(r -> Boolean.TRUE.equals(r.getIsSelectedForDocument())).findFirst();
        Routings route = mainCarriageRoutings.get(0);
        if (routings.isPresent()) {
            route = routings.get();
        }
        return (Constants.TRANSPORT_MODE_SEA.equals(route.getMode()) && StringUtility.isNotEmpty(route.getVesselName()) && StringUtility.isNotEmpty(route.getVoyage()))
                || (TRANSPORT_MODE_AIR.equals(route.getMode()) && StringUtility.isNotEmpty(route.getCarrier()) && StringUtility.isNotEmpty(route.getFlightNumber()));
    }
    public Map<String, RAKCDetailsResponse> getRAKCDetailsMap(List<String> addressIds) {
        if (CommonUtils.listIsNullOrEmpty(addressIds)) {
            return Collections.emptyMap();
        }

        CommonV1ListRequest request = convertV1InCriteriaRequest("Id", addressIds);
        V1DataResponse addressResponse = iv1Service.addressList(request);
        List<RAKCDetailsResponse> rakcDetailsList =
                jsonHelper.convertValueToList(addressResponse.getEntities(), RAKCDetailsResponse.class);

        return CommonUtils.listIsNullOrEmpty(rakcDetailsList)
                ? Collections.emptyMap()
                : rakcDetailsList.stream().collect(Collectors.toMap(rakc -> String.valueOf(rakc.getId()), Function.identity()));
    }

    public CommonV1ListRequest convertV1InCriteriaRequest(String filterValue, List<?> values) {
        List<String> itemType = new ArrayList<>();
        itemType.add(filterValue);
        List<List<?>> param = new ArrayList<>();
        param.add(values);
        List<Object> criteria = new ArrayList<>(Arrays.asList(itemType, "in", param));
        return CommonV1ListRequest.builder().criteriaRequests(criteria).build();
    }

    public void updateContainerTypeWithQuoteId(DependentServiceResponse dependentServiceResponse, String quoteId) {
        List<ContainerTypeMasterResponse> containerTypeMasterResponses = jsonHelper.convertValueToList(dependentServiceResponse.getData(), ContainerTypeMasterResponse.class);
        List<QuoteContracts> quoteContracts = quoteContractsDao.findByContractId(quoteId);
        List<String> quotedContainerTypes = quoteContracts.stream()
                .flatMap(qc -> qc.getContainerTypes().stream())
                .toList();
        containerTypeMasterResponses.forEach(response -> {
            if (quotedContainerTypes.contains(response.getCode())) {
                response.setIsQuoted(true);
            }
        });
        dependentServiceResponse.setData(containerTypeMasterResponses);
    }
    public EntityTransferAddress getEntityTransferAddress(TenantModel tenantModel) {
        if (Objects.nonNull(tenantModel.getDefaultAddressId())) {
            CommonV1ListRequest addressRequest = new CommonV1ListRequest();
            List<Object> addressField = new ArrayList<>(List.of("Id"));
            List<Object> addressCriteria = new ArrayList<>(List.of(addressField, "=", tenantModel.getDefaultAddressId()));
            addressRequest.setCriteriaRequests(addressCriteria);
            V1DataResponse addressResponse = v1Service.addressList(addressRequest);
            List<EntityTransferAddress> addressList = jsonHelper.convertValueToList(addressResponse.entities, EntityTransferAddress.class);
            return addressList.stream().findFirst().orElse(EntityTransferAddress.builder().build());
        }
        return null;
    }
    public String getAddress(Map<String, Object> addressData) {
        StringBuilder address = new StringBuilder(EMPTY_STRING);
        String[] keys = {COMPANY_NAME, ADDRESS1, CITY, STATE, ReportConstants.COUNTRY, ZIP_POST_CODE};

        for (String key : keys) {
            String value = (String) addressData.getOrDefault(key, EMPTY_STRING);
            if (StringUtility.isNotEmpty(value)) {
                address.append(value).append(ReportConstants.COMM);
            }
        }

        if (address.toString().endsWith(ReportConstants.COMM))
            address = new StringBuilder(address.substring(0, address.length() - COMM.length()));
        return address.toString();
    }

    public static String getSourceService() {
        try {
            return Objects.nonNull(MDC.get(Constants.ORIGINATED_FROM)) ? MDC.get(Constants.ORIGINATED_FROM) : Constants.SHIPMENT;
        } catch (Exception e) {
            log.error("{}, getSourceService: Message: {}", LoggerHelper.getRequestIdFromMDC(), e.getMessage());
            return Constants.SHIPMENT;
        }
    }

    /**
     * Fetches address data for the given address IDs.
     */
    public Map<Long, AddressDataV1> fetchAddressData(List<String> addressIdList) {
        if(!CommonUtils.listIsNullOrEmpty(addressIdList)) {
            CommonV1ListRequest addressRequest = createCriteriaToFetchAddressList(addressIdList);
            V1DataResponse addressResponse = v1Service.addressList(addressRequest);
            List<AddressDataV1> addressDataList = jsonHelper.convertValueToList(addressResponse.entities, AddressDataV1.class);

            return addressDataList.stream()
                    .collect(Collectors.toMap(AddressDataV1::getId, entity -> entity));
        }
        return new HashMap<>();
    }

    public CommonV1ListRequest createCriteriaToFetchAddressList(List<String> addressIdList) {
        List<Object> addressIdField = new ArrayList<>(List.of(ID));
        List<Object> addressCriteria = new ArrayList<>(List.of(addressIdField, Constants.IN, List.of(addressIdList)));
        return CommonV1ListRequest.builder().criteriaRequests(addressCriteria).build();
    }

    public Map<Long, OrgDataV1> fetchOrgAddressData(List<String> orgAddressIdList) {
        if(!CommonUtils.listIsNullOrEmpty(orgAddressIdList)) {
            CommonV1ListRequest orgAddressRequest = createCriteriaToFetchAddressList(orgAddressIdList);
            V1DataResponse addressResponse = v1Service.fetchOrganization(orgAddressRequest);
            List<OrgDataV1> addressDataList = jsonHelper.convertValueToList(addressResponse.entities, OrgDataV1.class);

            return addressDataList.stream()
                    .collect(Collectors.toMap(OrgDataV1::getId, entity -> entity));
        }
        return new HashMap<>();
    }

    public EntityTransferAddress getEntityTransferAddress(String transportMode) {
        try {
            TenantModel tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
            EntityTransferAddress entityTransferAddress = getEntityTransferAddress(tenantModel);
            if ((Constants.TRANSPORT_MODE_SEA.equals(transportMode)
                    || Constants.TRANSPORT_MODE_RAI.equals(transportMode)) && null != entityTransferAddress) {
                return entityTransferAddress;
            }
        } catch (Exception e) {
            log.error(e.getLocalizedMessage());
        }
        return null;
    }

    @Nullable
    public Long getLongValue(Object value) {
        if (value != null) {
            if (value instanceof Number) {
                return ((Number) value).longValue();
            } else if (value instanceof String) {
                return Long.parseLong((String) value);
            } else {
                throw new IllegalArgumentException("Unsupported Party ID type: " + value.getClass());
            }
        }
        return null;
    }

    public static String constructAddress(Map<String, Object> addressData, Map<String, Object> orgData) {
        StringBuilder sb = new StringBuilder();
        String newLine = "\r\n";

        if (addressData != null) {
            // Name
            if (addressData.containsKey(PartiesConstants.COMPANY_NAME)) {
                sb.append(StringUtility.toUpperCase(
                        StringUtility.convertToString(addressData.get(PartiesConstants.COMPANY_NAME))
                ));
            }
            else if (Objects.nonNull(orgData) && orgData.containsKey(PartiesConstants.FULLNAME)) {
                sb.append(StringUtility.toUpperCase(
                        StringUtility.convertToString(orgData.get(PartiesConstants.FULLNAME))
                ));
            }

            // Address1
            if (addressData.containsKey(PartiesConstants.ADDRESS1)) {
                sb.append(newLine).append(StringUtility.toUpperCase(
                        StringUtility.convertToString(addressData.get(PartiesConstants.ADDRESS1))
                ));
            }

            // Address2
            if (checkAddressKeyExists(addressData, PartiesConstants.ADDRESS2)) {
                sb.append(newLine).append(
                        StringUtility.toUpperCase(
                                StringUtility.convertToString(addressData.get(PartiesConstants.ADDRESS2))
                        )
                );
            }

            // City + State + Zip + Country in one line
            StringBuilder line3 = new StringBuilder(constructAddressL3(addressData));

            if (!line3.isEmpty()) {
                sb.append(newLine).append(line3);
            }
        }

        return sb.toString();
    }

    private static String constructAddressL3(Map<String, Object> addressData) {
        StringBuilder line3 = new StringBuilder();
        if (checkAddressKeyExists(addressData, PartiesConstants.CITY)) {
            line3.append(StringUtility.toUpperCase(
                    StringUtility.convertToString(addressData.get(PartiesConstants.CITY))
            ));
        }
        if (checkAddressKeyExists(addressData, PartiesConstants.STATE)) {
            if (!line3.isEmpty()) line3.append(" ");
            line3.append(StringUtility.toUpperCase(
                    StringUtility.convertToString(addressData.get(PartiesConstants.STATE))
            ));
        }
        if (checkAddressKeyExists(addressData, PartiesConstants.ZIP_POST_CODE)) {
            if (!line3.isEmpty()) line3.append(" ");
            line3.append(StringUtility.toUpperCase(
                    StringUtility.convertToString(addressData.get(PartiesConstants.ZIP_POST_CODE))
            ));
        }
        if (checkAddressKeyExists(addressData, PartiesConstants.COUNTRY)) {
            if (!line3.isEmpty()) line3.append(" ");
            line3.append(StringUtility.toUpperCase(
                    getCountryName(StringUtility.convertToString(addressData.get(PartiesConstants.COUNTRY)))
            ));
        }
        return line3.toString();
    }

    private static boolean checkAddressKeyExists(Map<String, Object> addressData, String key) {
        return addressData.containsKey(key) &&
                addressData.get(key) != null && StringUtility.isNotEmpty(String.valueOf(addressData.get(key)));
    }

    private static String getCountryName(String code) {
        if (StringUtility.isEmpty(code))
            return null;
        String countryName = CountryListHelper.ISO3166.getCountryNameByCode(code);
        return StringUtility.isNotEmpty(countryName) ? countryName : code;
    }

    public List<Map<String, Object>> buildFlatList(
            List<Object[]> results,
            List<String> columnOrder
    ) {
        List<Map<String, Object>> flatList = new ArrayList<>();

        for (Object[] row : results) {
            Map<String, Object> rowMap = new LinkedHashMap<>();
            for (int i = 0; i < columnOrder.size(); i++) {
                rowMap.put(columnOrder.get(i), row[i]);
            }
            flatList.add(rowMap);
        }

        return flatList;
    }
    public List<String> getAllSimpleFieldNames(Class<?> entityClass) {
        List<String> fields = Arrays.stream(entityClass.getDeclaredFields())
                .filter(f -> !Modifier.isStatic(f.getModifiers()))
                .filter(f -> !Modifier.isTransient(f.getModifiers()))
                .filter(f -> !"serialVersionUID".equals(f.getName()))
                .filter(f -> !Collection.class.isAssignableFrom(f.getType()))
                .filter(f -> !f.getType().isAnnotationPresent(Entity.class))
                .map(Field::getName)
                .collect(Collectors.toList());

        if (!fields.contains("id")) {
            fields.add(0, "id");
        } else {
            // Move id to first position if it exists
            fields.remove("id");
            fields.add(0, "id");
        }
        return  fields;
    }
    public Set<String> detectCollectionRelationships(Map<String, Object> requestedColumns, Class<?> entityClass) {
        Set<String> collections = new HashSet<>();

        for (String key : requestedColumns.keySet()) {
            try {
                Field field = entityClass.getDeclaredField(key);
                if (Collection.class.isAssignableFrom(field.getType())) {
                    collections.add(key);
                }
            } catch (NoSuchFieldException e) {
                // Not a direct field in ShipmentDetails (could be a joined OneToOne etc.), skip
            }
        }

        return collections;
    }
    public Map<String, Object> extractRequestedColumns(List<String> includeColumns, String mainEntityKey) {
        if (includeColumns.isEmpty()) {
            return new HashMap<>();
        }

        Set<String> validEntityKeys = ShipmentConstants.ENTITY_MAPPINGS.keySet();
        Map<String, Object> entityColumnsMap = new HashMap<>();

        for (String column : includeColumns) {
            processColumn(column, mainEntityKey, validEntityKeys, entityColumnsMap);
        }

        return entityColumnsMap;
    }

    private void processColumn(String column, String mainEntityKey, Set<String> validEntityKeys,
                               Map<String, Object> entityColumnsMap) {

        String[] parts = column.split("\\.");

        if (parts.length == 1) {
            extractCoulmnMap(mainEntityKey, validEntityKeys, parts, entityColumnsMap);
        } else {
            String topLevelEntity = parts[0];

            if (!validEntityKeys.contains(topLevelEntity)) {
                return; // Skip unknown entities
            }

            // Always create a list for each entity
            List<Object> entityList = (List<Object>) entityColumnsMap.computeIfAbsent(topLevelEntity, k -> new ArrayList<>());

            if (parts.length == 2) {
                // Simple field - add directly to list if not already present
                String fieldName = parts[1];
                if (!entityList.contains(fieldName)) {
                    entityList.add(fieldName);
                }
            } else {
                // Nested field - find or create the appropriate nested map
                String nestedEntityKey = parts[1]; // e.g., "notifyParty"
                String[] remainingParts = Arrays.copyOfRange(parts, 2, parts.length); // e.g., ["orgData"]

                // Look for existing nested map with this key
                Map<String, Object> nestedMap = findOrCreateNestedMap(entityList, nestedEntityKey);

                // Build the nested structure
                buildNestedStructure(nestedMap, nestedEntityKey, remainingParts);
            }
        }
    }

    private void buildNestedStructure(Map<String, Object> nestedMap, String key, String[] remainingParts) {
        if (remainingParts.length == 1) {
            // Final level - add to list
            List<String> fieldList = (List<String>) nestedMap.computeIfAbsent(key, k -> new ArrayList<>());
            String fieldName = remainingParts[0];
            if (!fieldList.contains(fieldName)) {
                fieldList.add(fieldName);
            }
        } else {
            // More nesting required
            Map<String, Object> nextLevel = (Map<String, Object>) nestedMap.computeIfAbsent(key, k -> new HashMap<>());
            String nextKey = remainingParts[0];
            String[] nextRemainingParts = Arrays.copyOfRange(remainingParts, 1, remainingParts.length);
            buildNestedStructure(nextLevel, nextKey, nextRemainingParts);
        }
    }

    private Map<String, Object> findOrCreateNestedMap(List<Object> entityList, String nestedEntityKey) {
        // Look for existing map that contains this nested entity key
        for (Object item : entityList) {
            if (item instanceof Map) {
                Map<String, Object> map = (Map<String, Object>) item;
                if (map.containsKey(nestedEntityKey)) {
                    return map;
                }
            }
        }

        // Create new map and add to list
        Map<String, Object> newMap = new HashMap<>();
        entityList.add(newMap);
        return newMap;
    }

    private static void extractCoulmnMap(String mainEntityKey, Set<String> validEntityKeys, String[] parts, Map<String, Object> entityColumnsMap) {
        if (validEntityKeys.contains(parts[0])) {
            entityColumnsMap.computeIfAbsent(parts[0], k -> new ArrayList<String>());
            // For child entities, empty list means "include all fields"
        }
        // Root-level field: belongs to shipmentDetails or mainEntityKey
        else if (validEntityKeys.contains(mainEntityKey)) {
            entityColumnsMap.computeIfAbsent(mainEntityKey, k -> new ArrayList<String>());
            List<String> fields = (List<String>) entityColumnsMap.get(mainEntityKey);
            fields.add(parts[0]);
        }
    }

    @SuppressWarnings("unchecked")
    private void buildNestedMap(Map<String, Object> currentMap, String[] parts, int index) {
        String current = parts[index];

        // Stop condition — add this part to the parent as a field
        if (ShipmentConstants.JSON_FIELDS.contains(current) || index == parts.length - 1) {
            currentMap.computeIfAbsent(current, k -> new ArrayList<>());
            return;
        }

        Object next = currentMap.get(current);

        // If next level exists and is a map, go deeper
        if (next instanceof Map) {
            buildNestedMap((Map<String, Object>) next, parts, index + 1);
        } else if (next instanceof List) {
            // Already a list, just add the new field if stop condition hit
            String stopField = parts[index + 1];
            List<String> list = (List<String>) next;
            if (!list.contains(stopField)) {
                list.add(stopField);
            }
        } else {
            // Create a map for the next level
            if (index + 2 < parts.length && !ShipmentConstants.JSON_FIELDS.contains(parts[index + 1])) {
                Map<String, Object> nextMap = new HashMap<>();
                currentMap.put(current, nextMap);
                buildNestedMap(nextMap, parts, index + 1);
            } else {
                // Next part is a terminal field, collect in list
                List<String> fieldList = new ArrayList<>();
                fieldList.add(parts[index + 1]);
                currentMap.put(current, fieldList);
            }
        }
    }



    public void fillEmptyColumnLists(Map<String, Object> requestedColumns) {
        Map<String, Class<?>> entityMappings = ShipmentConstants.ENTITY_MAPPINGS; // from our fixed Map.ofEntries(...)
        for (Map.Entry<String, Class<?>> e : entityMappings.entrySet()) {
            String key = e.getKey();
            Object value = ensureIdInCollection(requestedColumns.get(key));
            if (requestedColumns.containsKey(key)  && isEmptyOrNull(value)) {
                requestedColumns.put(key, getAllSimpleFieldNames(e.getValue()));
            }
        }
    }
    private boolean isEmptyOrNull(Object value) {
        if (value == null) return true;
        if (value instanceof List) return ((List<?>) value).isEmpty();
        if (value instanceof Map) return ((Map<?, ?>) value).isEmpty();
        if (value instanceof Set<?>) return ((Set<?>) value).isEmpty();
        return false;
    }
    public Object ensureIdInCollection(Object value) {
        if (value instanceof List) {
            List<String> list = (List<String>) value;
            if (!list.isEmpty() && !list.contains(ID)) {
                list.add(ID);
            }
            return list;
        } else if (value instanceof Set) {
            Set<String> set = (Set<String>) value;
            if(!set.isEmpty() && !set.contains(ID)) {
                set.add(ID);
            }
            return set;
        }
        return value;
    }
    // Helper method to convert flat map -> nested map
    public List<Map<String, Object>> convertToNestedMapWithCollections(
            List<Map<String, Object>> flatList,
            Set<String> collectionRelationships,
            String rootKey) {
        Map<Object, Map<String, Object>> parentMapById = new LinkedHashMap<>();
        String rootKeyValue = rootKey+Constants.DOT;
        for (Map<String, Object> flatRow : flatList) {
            Object parentId = flatRow.get(rootKeyValue+ID);

            Map<String, Object> entityMap =
                    parentMapById.computeIfAbsent(parentId, k -> new LinkedHashMap<>());

            // Initialize collection arrays
            for (String collKey : collectionRelationships) {
                entityMap.putIfAbsent(collKey, new ArrayList<>());
            }

            // Process each field
            for (Map.Entry<String, Object> entry : flatRow.entrySet()) {
                processFields(collectionRelationships, flatRow, entry, rootKeyValue, entityMap);
            }
        }

        // Convert to final format
        List<Map<String, Object>> finalList = new ArrayList<>();
        mapFinalList(rootKey, parentMapById, finalList);

        return finalList;
    }

    private static void processFields(Set<String> collectionRelationships, Map<String, Object> flatRow, Map.Entry<String, Object> entry, String rootKeyValue, Map<String, Object> entityMap) {
        String key = entry.getKey();
        Object value = entry.getValue();

        if (!key.startsWith(rootKeyValue) || value == null) {
            return;
        }

        String[] parts = key.substring(rootKeyValue.length()).split("\\.");

        if (parts.length == 1) {
            // Direct field on shipment
            entityMap.put(parts[0], value);
        } else if (parts.length == 2 && collectionRelationships.contains(parts[0])) {
            // Collection field like consolidationList.id
            handleNonNestedFields(flatRow, parts, entityMap, value, rootKeyValue);
        } else if (parts.length > 2 && collectionRelationships.contains(parts[0])) {
            // Nested fields within collections (e.g., consolidationList.address.city)
            handleNestedFields(flatRow, parts, entityMap, rootKeyValue, value);
        } else if (parts.length > 1) {
            // Regular nested objects (non-collection)
            handlerNestedNonCollectionObject(entityMap, parts, value);
        }
    }

    private static void mapFinalList(String rootKey, Map<Object, Map<String, Object>> parentMapById, List<Map<String, Object>> finalList) {
        for (Map<String, Object> data : parentMapById.values()) {
            finalList.add(Collections.singletonMap(rootKey, data));
        }
    }

    private static void handlerNestedNonCollectionObject(Map<String, Object> entityMap, String[] parts, Object value) {
        Map<String, Object> current = entityMap;
        for (int i = 0; i < parts.length - 1; i++) {
            current = (Map<String, Object>) current.computeIfAbsent(parts[i], k -> new LinkedHashMap<>());
        }
        current.put(parts[parts.length - 1], value);
    }

    private static void handleNestedFields(Map<String, Object> flatRow, String[] parts, Map<String, Object> entityMap, String rootKeyValue, Object value) {
        String collectionName = parts[0];

        List<Map<String, Object>> collection =
                (List<Map<String, Object>>) entityMap.get(collectionName);

        String idKey = rootKeyValue + collectionName + Constants.DOT+ID;
        Object childId = flatRow.get(idKey);

        if (childId != null) {
            Map<String, Object> childObj = collection.stream()
                    .filter(obj -> Objects.equals(obj.get(ID), childId))
                    .findFirst()
                    .orElseGet(() -> {
                        Map<String, Object> newObj = new LinkedHashMap<>();
                        newObj.put(ID, childId);
                        collection.add(newObj);
                        return newObj;
                    });

            // Build nested structure within child object
            Map<String, Object> current = childObj;
            for (int i = 1; i < parts.length - 1; i++) {
                current = (Map<String, Object>) current.computeIfAbsent(parts[i], k -> new LinkedHashMap<>());
            }
            current.put(parts[parts.length - 1], value);
        }
    }

    private static void handleNonNestedFields(Map<String, Object> flatRow, String[] parts, Map<String, Object> entityMap, Object value, String rootKeyValue) {
        String collectionName = parts[0];
        String fieldName = parts[1];

        List<Map<String, Object>> collection =
                (List<Map<String, Object>>) entityMap.get(collectionName);

        if (ID.equals(fieldName)) {
            // Find or create object with this ID
            boolean exists = collection.stream()
                    .anyMatch(obj -> Objects.equals(obj.get(ID), value));

            if (!exists) {
                Map<String, Object> newObj = new LinkedHashMap<>();
                newObj.put(ID, value);
                collection.add(newObj);
            }
        } else {
            // Find object by ID and set the field
            String idKey = rootKeyValue + collectionName + Constants.DOT+ID;
            Object childId = flatRow.get(idKey);

            if (childId != null) {
                Map<String, Object> childObj = collection.stream()
                        .filter(obj -> Objects.equals(obj.get(ID), childId))
                        .findFirst()
                        .orElseGet(() -> {
                            Map<String, Object> newObj = new LinkedHashMap<>();
                            newObj.put(ID, childId);
                            collection.add(newObj);
                            return newObj;
                        });
                childObj.put(fieldName, value);
            }
        }
    }

    public List<String> refineIncludeColumns(List<String> includeColumns) {
        List<String> requiredFields = Arrays.asList("id", "guid");
        requiredFields.stream()
                .filter(field -> !includeColumns.contains(field))
                .forEach(includeColumns::add);
        return includeColumns.stream()
                .map(column -> {
                    if (column.contains(Constants.ORG_DATA)) {
                        return column.substring(0, column.indexOf(Constants.ORG_DATA) + Constants.ORG_DATA.length());
                    } else if (column.contains(Constants.ADDRESS_DATA)) {
                        return column.substring(0, column.indexOf(Constants.ADDRESS_DATA) + Constants.ADDRESS_DATA.length());
                    } else {
                        return column;
                    }
                })
                .distinct() // remove duplicates if any
                .toList();
    }

    @SuppressWarnings("unchecked")
    public <T extends MultiTenancy> void buildJoinsAndSelections(
            Map<String, Object> requestedColumns,
            Root<T> root,
            List<Selection<?>> selections,
            List<String> columnOrder,
            String rootEntityKey,
            String sortField
    ) throws RunnerException {

        Set<String> rootEntityColumns = new HashSet<>((Collection) requestedColumns.getOrDefault(rootEntityKey, new ArrayList<>()));

        // If there's a sort field and it's not already in the root entity columns, validate and add it
        if (sortField != null && !rootEntityColumns.contains(sortField)) {
            if (isValidFieldForEntity(root, sortField)) {
                rootEntityColumns.add(sortField);
                Map<String, Object> updatedRequestedColumns = new HashMap<>(requestedColumns);
                updatedRequestedColumns.put(rootEntityKey, new ArrayList<>(rootEntityColumns));
                requestedColumns = updatedRequestedColumns;
                log.debug("Auto-included sort field '{}' for entity '{}'", sortField, rootEntityKey);
            } else {
                log.warn("Invalid sort field '{}' for entity type '{}'. Field does not exist in entity.", sortField, rootEntityKey);
                throw new RunnerException("Invalid sort field " + sortField);
            }
        }

        // Use a map to cache joins to avoid duplicate joins
        Map<String, Join<?, ?>> joinCache = new HashMap<>();

        for (Map.Entry<String, Object> entry : requestedColumns.entrySet()) {
            String entityKey = entry.getKey();
            Object value = entry.getValue();

            processEntity(value, rootEntityKey, root, selections, columnOrder, entityKey, joinCache);
        }
    }

    private void processEntity(Object value, String rootEntityKey, Root<?> root,
                               List<Selection<?>> selections, List<String> columnOrder,
                               String entityKey, Map<String, Join<?, ?>> joinCache) {
        if (value instanceof List) {
            processList(value, rootEntityKey, root, selections, columnOrder, entityKey, joinCache);
        } else if (value instanceof Map) {
            processNestedMap((Map<String, Object>) value, rootEntityKey, root, selections, columnOrder, entityKey, joinCache);
        }
    }

    public void processNestedMap(Map<String, Object> nestedMap, String rootEntityKey, Root<?> root,
                                 List<Selection<?>> selections, List<String> columnOrder,
                                 String parentEntityKey, Map<String, Join<?, ?>> joinCache) {

        for (Map.Entry<String, Object> entry : nestedMap.entrySet()) {
            String childEntityKey = entry.getKey();
            Object childValue = entry.getValue();

            // Build the full path for this nested entity
            String fullPath = parentEntityKey + "." + childEntityKey;
            processEntity(childValue, rootEntityKey, root, selections, columnOrder, fullPath, joinCache);
        }
    }

    public void processList(Object value, String rootEntityKey, Root<?> root,
                            List<Selection<?>> selections, List<String> columnOrder,
                            String entityPath, Map<String, Join<?, ?>> joinCache) {
        List<String> cols = (List<String>) value;

        if (cols == null || cols.isEmpty()) {
            return;
        }

        if (rootEntityKey.equals(entityPath)) {
            // Root entity: add columns directly from the root
            for (String col : cols) {
                selections.add(root.get(col));
                columnOrder.add(rootEntityKey + "." + col);
            }
        } else {
            // Handle nested joins
            Path<?> joinPath = buildJoinPath(root, entityPath, joinCache);

            for (String col : cols) {
                selections.add(joinPath.get(col));
                columnOrder.add(rootEntityKey + "." + entityPath + "." + col);
            }
        }
    }

    private Path<?> buildJoinPath(Root<?> root, String entityPath, Map<String, Join<?, ?>> joinCache) {
        // Check if we already have this join cached
        if (joinCache.containsKey(entityPath)) {
            return joinCache.get(entityPath);
        }

        String[] pathParts = entityPath.split("\\.");
        Path<?> currentPath = root;
        // Build the join path step by step
        StringBuilder currentPathBuilder = new StringBuilder();
        for (String part : pathParts) {
            if (currentPathBuilder.length() > 0) {
                currentPathBuilder.append(".");
            }
            currentPathBuilder.append(part);
            String currentFullPath = currentPathBuilder.toString();

            if (joinCache.containsKey(currentFullPath)) {
                currentPath = joinCache.get(currentFullPath);
            } else {
                // Create new join
                Join<?, ?> join;
                if (currentPath instanceof Root) {
                    join = ((Root<?>) currentPath).join(part, JoinType.LEFT);
                } else {
                    join = ((Join<?, ?>) currentPath).join(part, JoinType.LEFT);
                }
                joinCache.put(currentFullPath, join);
                currentPath = join;
            }
        }

        return currentPath;
    }

    public <T extends MultiTenancy> long fetchTotalCount(ListCommonRequest listCommonRequest, Class<T> entityClass) {
        CriteriaBuilder cb = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> countQuery = cb.createQuery(Long.class);
        Root<T> root = countQuery.from(entityClass);

        // Build predicates same way as main query
        List<Predicate> predicates = buildPredicatesFromFilters(cb, root, listCommonRequest);

        countQuery.select(cb.countDistinct(root)); // count distinct root entities
        if (!predicates.isEmpty()) {
            countQuery.where(cb.and(predicates.toArray(new Predicate[0])));
        }

        return entityManager.createQuery(countQuery).getSingleResult();
    }
    public String extractSortFieldFromPayload(ListCommonRequest requestPayload) {
        if (requestPayload == null || requestPayload.getSortRequest() == null) {
            return null;
        }

        SortRequest sortRequest = requestPayload.getSortRequest();
        return sortRequest.getFieldName();
    }

    private <T extends MultiTenancy> boolean isValidFieldForEntity(Root<T> root, String fieldName) {
        try {
            // Try to access the field using JPA metamodel - this will throw exception if field doesn't exist
            root.get(fieldName);
            return true;
        } catch (IllegalArgumentException e) {
            // Field doesn't exist in the entity
            log.debug("Field '{}' does not exist in entity type '{}'", fieldName, root.getJavaType().getSimpleName());
            return false;
        }
    }

    public <T extends MultiTenancy> List<Predicate> buildPredicatesFromFilters(
            CriteriaBuilder cb,
            Root<T> root,
            ListCommonRequest listCommonRequest) {
        List<Predicate> predicates = new ArrayList<>();
        predicates.add(cb.equal(root.get("tenantId"), UserContext.getUser().getTenantId()));
        // Create a join cache for filter predicates too
        Map<String, Join<?, ?>> filterJoinCache = new HashMap<>();
        List<FilterCriteria> filterCriteria = listCommonRequest.getFilterCriteria();

        for (FilterCriteria group : filterCriteria) {
            Predicate innerPredicate = buildInnerPredicate(cb, root, group.getInnerFilter(), filterJoinCache);
            if (innerPredicate != null) {
                predicates.add(innerPredicate);
            }
        }
        return predicates;
    }

    private Predicate buildInnerPredicate(
            CriteriaBuilder cb,
            Root<?> root,
            List<FilterCriteria> innerFilterObj,
            Map<String, Join<?, ?>> filterJoinCache) {
        Predicate innerPredicate = null;
        for (FilterCriteria inner : innerFilterObj) {
            Criteria critMap = inner.getCriteria();
            String fieldName = critMap.getFieldName();
            String operator = critMap.getOperator();
            Object value = critMap.getValue();

            Predicate p = buildPredicateForOperator(cb, root, fieldName, operator, value, filterJoinCache);
            innerPredicate = calculateInnerPredicate(cb, inner, innerPredicate, p);
        }
        return innerPredicate;
    }

    private Predicate buildPredicateForOperator(
            CriteriaBuilder cb,
            Root<?> root,
            String fieldName,
            String operator,
            Object value,
            Map<String, Join<?, ?>> filterJoinCache) {

        Path<?> fieldPath = getFieldPath(root, fieldName, filterJoinCache);

        return switch (operator.trim().toLowerCase()) {
            case "=" -> cb.equal(fieldPath, value);
            case "!=" -> cb.notEqual(fieldPath, value);
            case "like" -> {
                Expression<String> stringPath = fieldPath.as(String.class);
                yield cb.like(stringPath, "%" + value + "%");
            }
            case ">" -> buildGreaterThanPredicate(cb, fieldPath, value);
            case "<" -> buildLessThanPredicate(cb, fieldPath, value);
            case ">=" -> buildGreaterThanOrEqualPredicate(cb, fieldPath, value);
            case "<=" -> buildLessThanOrEqualPredicate(cb, fieldPath, value);
            case "contains" -> {
                Expression<String> stringPath = fieldPath.as(String.class);
                yield cb.like(cb.lower(stringPath), "%" + value.toString().toLowerCase() + "%");
            }
            case "notlike" -> {
                Expression<String> stringPath = fieldPath.as(String.class);
                yield cb.notLike(cb.lower(stringPath), "%" + value.toString().toLowerCase() + "%");
            }
            case "startswith" -> {
                Expression<String> stringPath = fieldPath.as(String.class);
                yield cb.like(cb.lower(stringPath), value.toString().toLowerCase() + "%");
            }
            case "endswith" -> {
                Expression<String> stringPath = fieldPath.as(String.class);
                yield cb.like(cb.lower(stringPath), "%" + value.toString().toLowerCase());
            }
            case "in" -> fieldPath.in((Collection<?>) value);
            case "notin" -> cb.not(fieldPath.in((Collection<?>) value));
            case "isnull" -> cb.isNull(fieldPath);
            case "isnotnull" -> cb.isNotNull(fieldPath);
            default -> null;
        };
    }

    private Path<?> getFieldPath(Root<?> root, String fieldName, Map<String, Join<?, ?>> filterJoinCache) {
        if (!fieldName.contains(".")) {
            // Simple field on root entity
            return root.get(fieldName);
        }

        // Nested field - build join path
        String[] parts = fieldName.split("\\.");
        String fieldPart = parts[parts.length - 1]; // Last part is the actual field
        String joinPath = String.join(".", Arrays.copyOf(parts, parts.length - 1)); // Everything except last part

        // Build or get the join path
        Path<?> joinedPath = buildJoinPath(root, joinPath, filterJoinCache);

        // Return the field from the joined path
        return joinedPath.get(fieldPart);
    }

    @SuppressWarnings("unchecked")
    private Predicate buildGreaterThanPredicate(
            CriteriaBuilder cb,
            Path<?> fieldPath,
            Object value) {
        if (value instanceof String string) {
            Expression<String> stringPath = fieldPath.as(String.class);
            return cb.greaterThan(stringPath, string);
        } else if (value instanceof Number number) {
            Expression<Number> numberPath = fieldPath.as(Number.class);
            return cb.gt(numberPath, number);
        } else if (value instanceof Date date) {
            Expression<Date> datePath = fieldPath.as(Date.class);
            return cb.greaterThan(datePath, date);
        } else if (value instanceof LocalDateTime localDateTime) {
            Expression<LocalDateTime> dateTimePath = fieldPath.as(LocalDateTime.class);
            return cb.greaterThan(dateTimePath, localDateTime);
        }else if (value instanceof Comparable<?> comparable) {
            // Generic comparable handling
            Expression<? extends Comparable<?>> comparablePath = (Expression<? extends Comparable<?>>) fieldPath;
            return cb.lessThanOrEqualTo((Expression<Comparable<Object>>) comparablePath, (Comparable<Object>) comparable);
        }else {
            throw new IllegalArgumentException("Unsupported type for > operator: " + value.getClass());
        }
    }

    @SuppressWarnings("unchecked")
    private Predicate buildLessThanPredicate(
            CriteriaBuilder cb,
            Path<?> fieldPath,
            Object value) {
        if (value instanceof String string) {
            Expression<String> stringPath = fieldPath.as(String.class);
            return cb.lessThan(stringPath, string);
        } else if (value instanceof Number number) {
            Expression<Number> numberPath = fieldPath.as(Number.class);
            return cb.lt(numberPath, number);
        } else if (value instanceof Date date) {
            Expression<Date> datePath = fieldPath.as(Date.class);
            return cb.lessThan(datePath, date);
        } else if (value instanceof LocalDateTime localDateTime) {
            Expression<LocalDateTime> dateTimePath = fieldPath.as(LocalDateTime.class);
            return cb.lessThan(dateTimePath, localDateTime);
        }else if (value instanceof Comparable<?> comparable) {
            // Generic comparable handling
            Expression<? extends Comparable<?>> comparablePath = (Expression<? extends Comparable<?>>) fieldPath;
            return cb.lessThan((Expression<Comparable<Object>>) comparablePath, (Comparable<Object>) comparable);
        } else {
            throw new IllegalArgumentException("Unsupported type for < operator: " + value.getClass());
        }
    }

    @SuppressWarnings("unchecked")
    private Predicate buildGreaterThanOrEqualPredicate(
            CriteriaBuilder cb,
            Path<?> fieldPath,
            Object value) {
        if (value instanceof String string) {
            Expression<String> stringPath = fieldPath.as(String.class);
            return cb.greaterThanOrEqualTo(stringPath, string);
        } else if (value instanceof Number number) {
            Expression<Number> numberPath = fieldPath.as(Number.class);
            return cb.ge(numberPath, number);
        } else if (value instanceof Date date) {
            Expression<Date> datePath = fieldPath.as(Date.class);
            return cb.greaterThanOrEqualTo(datePath, date);
        } else if (value instanceof LocalDateTime localDateTime) {
            Expression<LocalDateTime> dateTimePath = fieldPath.as(LocalDateTime.class);
            return cb.greaterThanOrEqualTo(dateTimePath, localDateTime);
        } else if (value instanceof Comparable<?> comparable) {
            // Generic comparable handling
            Expression<? extends Comparable<?>> comparablePath = (Expression<? extends Comparable<?>>) fieldPath;
            return cb.greaterThanOrEqualTo((Expression<Comparable<Object>>) comparablePath, (Comparable<Object>) comparable);
        } else {
            throw new IllegalArgumentException("Unsupported type for >= operator: " + value.getClass());
        }
    }

    @SuppressWarnings("unchecked")
    private Predicate buildLessThanOrEqualPredicate(
            CriteriaBuilder cb,
            Path<?> fieldPath,
            Object value) {
        if (value instanceof String string) {
            Expression<String> stringPath = fieldPath.as(String.class);
            return cb.lessThanOrEqualTo(stringPath, string);
        } else if (value instanceof Number number) {
            Expression<Number> numberPath = fieldPath.as(Number.class);
            return cb.le(numberPath, number);
        } else if (value instanceof Date date) {
            Expression<Date> datePath = fieldPath.as(Date.class);
            return cb.lessThanOrEqualTo(datePath, date);
        } else if (value instanceof LocalDateTime localDateTime) {
            Expression<LocalDateTime> dateTimePath = fieldPath.as(LocalDateTime.class);
            return cb.lessThanOrEqualTo(dateTimePath, localDateTime);
        } else if (value instanceof Comparable<?> comparable) {
            // Generic comparable handling
            Expression<? extends Comparable<?>> comparablePath = (Expression<? extends Comparable<?>>) fieldPath;
            return cb.lessThanOrEqualTo((Expression<Comparable<Object>>) comparablePath, (Comparable<Object>) comparable);
        } else {
            throw new IllegalArgumentException("Unsupported type for <= operator: " + value.getClass());
        }
    }
    private Predicate calculateInnerPredicate(CriteriaBuilder cb, FilterCriteria inner, Predicate innerPredicate, Predicate p) {
        if (innerPredicate == null) {
            innerPredicate = p;
        } else {
            String logic = Optional.ofNullable(inner.getLogicOperator())
                    .map(Object::toString)
                    .orElse("and");
            if ("or".equalsIgnoreCase(logic)) {
                innerPredicate = cb.or(innerPredicate, p);
            } else {
                innerPredicate = cb.and(innerPredicate, p);
            }
        }
        return innerPredicate;
    }

    public void checkPermissionsForCloning(ShipmentDetails shipmentDetails) {
        ShipmentSettingsDetails shipmentSettingsDetails = getShipmentSettingFromContext();
        Boolean countryAirCargoSecurity = shipmentSettingsDetails.getCountryAirCargoSecurity();
        if (Boolean.TRUE.equals(countryAirCargoSecurity) && !CommonUtils.checkAirSecurityForShipment(shipmentDetails)) {
            throw new ValidationException(Constants.AIR_SECURITY_PERMISSION_MSG);
        }
    }

    public <T> void mapIfSelected(boolean flag, T value, Consumer<T> setter) {
        if (flag && value != null) {
            setter.accept(value);
        }
    }

    public <T> void mapIfSelected(T value, Consumer<T> setter) {
        if (value != null) {
            setter.accept(value);
        }
    }

    public PartiesResponse getPartiesResponse(Parties partyData) {
        PartiesResponse partiesResponse = new PartiesResponse();
        if (null != partyData){
            partiesResponse.setEntityId(partyData.getEntityId());
            partiesResponse.setEntityType(partyData.getEntityType());
            partiesResponse.setTenantId(partyData.getTenantId());
            partiesResponse.setType(partyData.getType());
            partiesResponse.setOrgCode(partyData.getOrgCode());
            partiesResponse.setAddressCode(partyData.getAddressCode());
            partiesResponse.setOrgId(partyData.getOrgId());
            partiesResponse.setAddressId(partyData.getAddressId());
            partiesResponse.setOrgData(partyData.getOrgData());
            partiesResponse.setAddressData(partyData.getAddressData());
            partiesResponse.setIsAddressFreeText(partyData.getIsAddressFreeText());
            partiesResponse.setCountryCode(partyData.getCountryCode());
        }
        return partiesResponse;
    }
    public static boolean canFetchDetailsWithoutTenantFilter(String xSource) {
        if (Objects.equals(xSource, NETWORK_TRANSFER)) {
            return true;
        }

        return Objects.equals(xSource, CROSS_TENANT_SOURCE) &&
               Optional.ofNullable(UserContext.getUser())
                .map(UsersDto::getPermissions)
                .map(permissions -> Boolean.TRUE.equals(permissions.get(CAN_VIEW_ALL_BRANCH_SHIPMENTS)))
                .orElse(false);
    }

    public boolean isSelectedModeOffInBooking(String transportMode, V1TenantSettingsResponse tenantData) {
        return switch (transportMode) {
            case TRANSPORT_MODE_AIR -> Boolean.FALSE.equals(tenantData.getBookingTransportModeAir());
            case TRANSPORT_MODE_SEA -> Boolean.FALSE.equals(tenantData.getBookingTransportModeSea());
            case TRANSPORT_MODE_RAI -> Boolean.FALSE.equals(tenantData.getBookingTransportModeRail());
            case TRANSPORT_MODE_ROA -> Boolean.FALSE.equals(tenantData.getBookingTransportModeRoad());
            default -> throw new IllegalArgumentException("Unknown transport mode: " + transportMode);
        };
    }

    public boolean isSelectedModeOffInShipment(String transportMode, V1TenantSettingsResponse tenantData) {
        return switch (transportMode) {
            case TRANSPORT_MODE_AIR -> Boolean.FALSE.equals(tenantData.getShipmentTransportModeAir());
            case TRANSPORT_MODE_SEA -> Boolean.FALSE.equals(tenantData.getShipmentTransportModeSea());
            case TRANSPORT_MODE_RAI -> Boolean.FALSE.equals(tenantData.getShipmentTransportModeRail());
            case TRANSPORT_MODE_ROA -> Boolean.FALSE.equals(tenantData.getShipmentTransportModeRoad());
            default -> throw new IllegalArgumentException("Unknown transport mode: " + transportMode);
        };
    }

    public void validateAirSecurityPermission(String transportType, String direction) {
        ShipmentSettingsDetails shipmentSettingsDetails = getShipmentSettingFromContext();
        Boolean countryAirCargoSecurity = shipmentSettingsDetails.getCountryAirCargoSecurity();
        if (Boolean.TRUE.equals(countryAirCargoSecurity) && !checkAirSecurityForTransportTypeAndDirection(transportType, direction)) {
            throw new ValidationException(AIR_SECURITY_PERMISSION_MSG);
        }
    }

    public boolean checkAirSecurityForTransportTypeAndDirection(String transportType, String direction) {
        if (transportType.equals(Constants.TRANSPORT_MODE_AIR) && direction.equals(DIRECTION_EXP)) {
            return UserContext.isAirSecurityUser();
        }
        return true;
    }

    public CloneFieldResponse getCloneFieldResponse(String type) {
        try {
            return objectMapper.readValue(StringUtility.convertToString(applicationConfigService.getValue(type)), CloneFieldResponse.class);
        } catch (Exception e) {
            throw new ValidationException("Invalid Type");
        }
    }

    public LocalDateTime convertToLocalDateTimeFromInttra(String dateValue, String dateFormat) {
        if (dateValue == null || dateValue.trim().isEmpty()) {
            return null;
        }

        if (dateFormat == null || dateFormat.trim().isEmpty()) {
            log.error("Date format cannot be null or empty");
            return null;
        }

        dateValue = dateValue.trim();
        dateFormat = dateFormat.trim().toUpperCase();

        try {
            if ("CCYYMMDDHHMM".equals(dateFormat)) {
                return parseCCYYMMDDHHMM(dateValue);
            } else if ("CCYYMMDD".equals(dateFormat)) {
                return parseCCYYMMDD(dateValue);
            } else {
                log.error("Unsupported date format: {}", dateFormat);
                return null;
            }
        } catch (Exception e) {
            log.error("Error parsing date value: {} with format: {} - {}", dateValue, dateFormat, e.getMessage());
            return null;
        }
    }

    private LocalDateTime parseCCYYMMDDHHMM(String dateValue) {
        if (dateValue.length() != 12) {
            log.error("Date value length must be 12 for CCYYMMDDHHMM format, but was {}, value: {}",
                    dateValue.length(), dateValue);
            return null;
        }

        // Parse CCYYMMDDHHMM format: 202008281530
        String year = dateValue.substring(0, 4);    // 2020
        String month = dateValue.substring(4, 6);   // 08
        String day = dateValue.substring(6, 8);     // 28
        String hour = dateValue.substring(8, 10);   // 15
        String minute = dateValue.substring(10, 12); // 30

        // Validate and convert
        int yearInt = Integer.parseInt(year);
        int monthInt = Integer.parseInt(month);
        int dayInt = Integer.parseInt(day);
        int hourInt = Integer.parseInt(hour);
        int minuteInt = Integer.parseInt(minute);

        if (!isValidDate(yearInt, monthInt, dayInt) || !isValidTime(hourInt, minuteInt)) {
            return null;
        }

        return LocalDateTime.of(yearInt, monthInt, dayInt, hourInt, minuteInt);
    }

    private LocalDateTime parseCCYYMMDD(String dateValue) {
        if (dateValue.length() != 8) {
            log.error("Date value length must be 8 for CCYYMMDD format, but was {}, value: {}",
                    dateValue.length(), dateValue);
            return null;
        }

        // Parse CCYYMMDD format: 20200828
        String year = dateValue.substring(0, 4);    // 2020
        String month = dateValue.substring(4, 6);   // 08
        String day = dateValue.substring(6, 8);     // 28

        // Validate and convert
        int yearInt = Integer.parseInt(year);
        int monthInt = Integer.parseInt(month);
        int dayInt = Integer.parseInt(day);

        if (!isValidDate(yearInt, monthInt, dayInt)) {
            return null;
        }

        // Default time to start of day (00:00)
        return LocalDateTime.of(yearInt, monthInt, dayInt, 0, 0);
    }

    private boolean isValidDate(int year, int month, int day) {
        if (month < 1 || month > 12) {
            log.error("Invalid month: {}", month);
            return false;
        }
        if (day < 1 || day > 31) {
            log.error("Invalid day: {}", day);
            return false;
        }
        log.debug("year: {}, month: {}, day: {}", year, month, day);
        // Additional validation for days in month could be added here
        // For now, basic range validation is sufficient

        return true;
    }

    private boolean isValidTime(int hour, int minute) {
        if (hour < 0 || hour > 23) {
            log.error("Invalid hour: {}", hour);
            return false;
        }
        if (minute < 0 || minute > 59) {
            log.error("Invalid minute: {}", minute);
            return false;
        }
        return true;
    }


    public String convertSeconds(long totalSeconds) {
        long minutes = totalSeconds / 60;
        long seconds = totalSeconds % 60;

        if (minutes == 0) {
            return Constants.EXPORT_EXCEL_MESSAGE + seconds + " seconds. Please try again after that time.";
        } else if (seconds == 0) {
            return Constants.EXPORT_EXCEL_MESSAGE + minutes + " minutes. Please try again after that time.";
        } else {
            return Constants.EXPORT_EXCEL_MESSAGE + minutes + " minutes and " + seconds + " seconds. Please try again after that time.";
        }
    }

    public void validateAndSetOriginAndDestinationPortIfNotExist(ShipmentDetails shipment, ConsolidationDetails console) {

        CarrierDetails carrierDetails;
        List<Routings> routings;
        if (null != shipment) {
            carrierDetails = shipment.getCarrierDetails();
            routings = shipment.getRoutingsList();
        } else {
            carrierDetails = console.getCarrierDetails();
            routings = console.getRoutingsList();
        }

        Set<String> plcData = new HashSet<>();
        boolean isCarrierLocCodeAdded = false;
        boolean isRoutingLocCodeAdded = false;
        isCarrierLocCodeAdded = validateCarrierDetail(carrierDetails, isCarrierLocCodeAdded, plcData);
        isRoutingLocCodeAdded = validateRoute(routings, isRoutingLocCodeAdded, plcData);
        setIfLocCodeExist(plcData, isCarrierLocCodeAdded, carrierDetails, isRoutingLocCodeAdded, routings);
    }

    private static boolean validateCarrierDetail(CarrierDetails carrierDetails, boolean isCarrierLocCodeAdded, Set<String> plcData) {
        if (null != carrierDetails) {
            if (null == carrierDetails.getOriginPortLocCode() && null != carrierDetails.getOriginPort()) {
                isCarrierLocCodeAdded = true;
                plcData.add(carrierDetails.getOriginPort());
            }
            if (null == carrierDetails.getDestinationPortLocCode() && null != carrierDetails.getDestinationPort()) {
                isCarrierLocCodeAdded = true;
                plcData.add(carrierDetails.getDestinationPort());
            }
        }
        return isCarrierLocCodeAdded;
    }

    private static boolean validateRoute(List<Routings> routings, boolean isRoutingLocCodeAdded, Set<String> plcData) {
        if (Objects.isNull(routings) || routings.isEmpty()) {
            return isRoutingLocCodeAdded;
        }
        for (Routings route : routings) {
            if (null == route.getOriginPortLocCode() && null != route.getPol()){
                isRoutingLocCodeAdded = true;
                plcData.add(route.getPol());
            }
            if (null == route.getDestinationPortLocCode() && null != route.getPod()){
                isRoutingLocCodeAdded = true;
                plcData.add(route.getPod());
            }
        }
        return isRoutingLocCodeAdded;
    }

    private void setIfLocCodeExist(Set<String> plcData, boolean isCarrierLocCodeAdded, CarrierDetails carrierDetails, boolean isRoutingLocCodeAdded, List<Routings> routings) {

        if (!CollectionUtils.isEmpty(plcData)) {
            log.info("Getting unLocationData from v1 for plcData : {}", plcData.stream().toList());
            Map<String, UnlocationsResponse> unlocationsMap = masterDataUtils.getLocationData(plcData);
            if (!unlocationsMap.isEmpty()) {
                unlocationsMap.forEach((key, value) ->
                    log.info("UnlocCode for : {} is  : {} ", key, value.getLocCode())
                );
            }
            if (isCarrierLocCodeAdded && Objects.nonNull(carrierDetails)) {
                setCarrierData(carrierDetails, unlocationsMap);
            }
            if (isRoutingLocCodeAdded && Objects.nonNull(routings) && !routings.isEmpty()) {
                setRoutingData(routings, unlocationsMap);
            }
        }
    }

    private static void setCarrierData(CarrierDetails carrierDetails, Map<String, UnlocationsResponse> unlocationsMap) {
        if (null == carrierDetails.getOriginPortLocCode() && null != carrierDetails.getOriginPort() && unlocationsMap.containsKey(carrierDetails.getOriginPort())) {
            UnlocationsResponse unLocResp = unlocationsMap.get(carrierDetails.getOriginPort());
            carrierDetails.setOriginPortLocCode(unLocResp.getLocCode());
        }
        if (null == carrierDetails.getDestinationPortLocCode() && null != carrierDetails.getDestinationPort() && unlocationsMap.containsKey(carrierDetails.getDestinationPort())) {
            UnlocationsResponse unLocResp = unlocationsMap.get(carrierDetails.getDestinationPort());
            carrierDetails.setDestinationPortLocCode(unLocResp.getLocCode());
        }
    }

    private static void setRoutingData(List<Routings> routings, Map<String, UnlocationsResponse> unlocationsMap) {
        for (Routings route : routings) {
            if (null == route.getOriginPortLocCode() && null != route.getPol() && unlocationsMap.containsKey(route.getPol())){
                UnlocationsResponse unLocResp = unlocationsMap.get(route.getPol());
                route.setOriginPortLocCode(unLocResp.getLocCode());
            }
            if (null == route.getDestinationPortLocCode() && null != route.getPod() && unlocationsMap.containsKey(route.getPod())){
                UnlocationsResponse unLocResp = unlocationsMap.get(route.getPod());
                route.setDestinationPortLocCode(unLocResp.getLocCode());
            }
        }
    }

    public boolean getBooleanConfigFromAppConfig(String appConfigKey) {
        String configuredValue = applicationConfigService.getValue(appConfigKey);
        if (null == configuredValue) {
            return false;
        }
        return "true".equalsIgnoreCase(configuredValue);
    }


    public Map<Long, String> getTenantNameMap(List<Integer> tenantIds){
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> field = new ArrayList<>(List.of("TenantId"));
        String operator = Operators.IN.getValue();
        List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(tenantIds)));
        request.setCriteriaRequests(criteria);
        V1DataResponse tenantName = v1Service.tenantNameByTenantId(request);

        List<V1TenantResponse> v1TenantResponse = jsonHelper.convertValueToList(tenantName.entities, V1TenantResponse.class);
        return v1TenantResponse.stream()
                .filter(response -> response.getTenantId() != null && response.getTenantName() != null)
                .collect(Collectors.toMap(
                        V1TenantResponse::getTenantId,
                        V1TenantResponse::getTenantName
                ));
    }
}