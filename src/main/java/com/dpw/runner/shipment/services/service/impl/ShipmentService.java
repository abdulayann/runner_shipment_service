package com.dpw.runner.shipment.services.service.impl;


import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.KCRA_EXPIRY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SRN;
import static com.dpw.runner.shipment.services.commons.constants.ApplicationConfigConstants.EXPORT_EXCEL_LIMIT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.AIR_DG_CONSOLIDATION_NOT_ALLOWED_WITH_INTER_BRANCH_SHIPMENT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.AIR_DG_SHIPMENT_NOT_ALLOWED_WITH_INTER_BRANCH_CONSOLIDATION;
import static com.dpw.runner.shipment.services.commons.constants.Constants.AUTO_REJECTION_REMARK;
import static com.dpw.runner.shipment.services.commons.constants.Constants.BOOKINGS_WITH_SQ_BRACKETS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CAN_NOT_ATTACH_MORE_SHIPMENTS_IN_DG_CONSOL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CAN_NOT_UPDATE_DG_SHIPMENTS_CONSOLE_CONSISTS_MULTIPLE_SHIPMENTS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARGO_TYPE_FCL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSOLIDATION;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONTAINS_HAZARDOUS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CREATED_AT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DIRECTION_CTS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DIRECTION_EXP;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ERROR_WHILE_SENDING_EMAIL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ID;
import static com.dpw.runner.shipment.services.commons.constants.Constants.IMPORT_SHIPMENT_PUSH_ATTACHMENT_EMAIL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.MASS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.MPK;
import static com.dpw.runner.shipment.services.commons.constants.Constants.OCEAN_DG_CONTAINER_FIELDS_VALIDATION;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENTS_WITH_SQ_BRACKETS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_TYPE_BCN;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_TYPE_DRT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_TYPE_HSE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_TYPE_LCL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_TYPE_SCN;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_TYPE_STD;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_AIR;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_SEA;
import static com.dpw.runner.shipment.services.commons.constants.Constants.VOLUME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.VOLUME_UNIT_M3;
import static com.dpw.runner.shipment.services.commons.constants.Constants.WEIGHT_UNIT_KG;
import static com.dpw.runner.shipment.services.commons.constants.ShipmentConstants.PADDING_10_PX;
import static com.dpw.runner.shipment.services.commons.constants.ShipmentConstants.STYLE;
import static com.dpw.runner.shipment.services.commons.enums.DBOperationType.COMMERCIAL_APPROVE;
import static com.dpw.runner.shipment.services.commons.enums.DBOperationType.COMMERCIAL_REQUEST;
import static com.dpw.runner.shipment.services.commons.enums.DBOperationType.DG_APPROVE;
import static com.dpw.runner.shipment.services.commons.enums.DBOperationType.DG_REQUEST;
import static com.dpw.runner.shipment.services.entity.enums.DateBehaviorType.ACTUAL;
import static com.dpw.runner.shipment.services.entity.enums.DateBehaviorType.ESTIMATED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_ACCEPTED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_COMMERCIAL_ACCEPTED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_COMMERCIAL_REJECTED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_COMMERCIAL_REQUESTED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_REJECTED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_REQUESTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus.SAILED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PULL_ACCEPTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PULL_REJECTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PULL_WITHDRAW;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PUSH_ACCEPTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PUSH_REJECTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PUSH_WITHDRAW;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.andCriteria;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListRequestFromEntityId;
import static com.dpw.runner.shipment.services.utils.CommonUtils.getIntFromString;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.listIsNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.setIsNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.StringUtility.isNotEmpty;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.adapters.impl.BillingServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.IOrderManagementAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.LicenseContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.DpsConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.constants.MdmConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.enums.ModuleValidationFieldType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.commons.requests.UpdateConsoleShipmentRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerPartialListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.config.LocalTimeZoneHelper;
import com.dpw.runner.shipment.services.dao.interfaces.IAdditionalDetailDao;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IBookingCarriageDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICommonErrorLogsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IELDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IHblDao;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotificationDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPickupDeliveryDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IQuartzJobInfoDao;
import com.dpw.runner.shipment.services.dao.interfaces.IReferenceNumbersDao;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.ISequenceIncrementorDao;
import com.dpw.runner.shipment.services.dao.interfaces.IServiceDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentOrderDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITruckDriverDetailsDao;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerUpdateFileEntitiesRequest;
import com.dpw.runner.shipment.services.document.response.DocumentManagerResponse;
import com.dpw.runner.shipment.services.document.service.IDocumentManagerService;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.AssignAllDialogDto;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.AutoUpdateWtVolRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.AutoUpdateWtVolResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculateContainerSummaryRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackSummaryRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackUtilizationRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculateShipmentSummaryRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculateShipmentSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerAssignListRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentConsoleIdDto;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentContainerAssignRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentMeasurementDetailsDto;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.mapper.AttachListShipmentMapper;
import com.dpw.runner.shipment.services.dto.mapper.ShipmentMapper;
import com.dpw.runner.shipment.services.dto.patchrequest.CarrierPatchRequest;
import com.dpw.runner.shipment.services.dto.patchrequest.ShipmentPatchRequest;
import com.dpw.runner.shipment.services.dto.request.AdditionalDetailRequest;
import com.dpw.runner.shipment.services.dto.request.ArrivalDepartureDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.AttachListShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.BookingCarriageRequest;
import com.dpw.runner.shipment.services.dto.request.CarrierDetailRequest;
import com.dpw.runner.shipment.services.dto.request.CheckCreditLimitFromV1Request;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.request.ELDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.LogHistoryRequest;
import com.dpw.runner.shipment.services.dto.request.NotesRequest;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.PickupDeliveryDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.ServiceDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentOrderAttachDetachRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentOrderRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.TrackingRequest;
import com.dpw.runner.shipment.services.dto.request.TruckDriverDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.billing.InvoicePostingValidationRequest;
import com.dpw.runner.shipment.services.dto.request.notification.PendingNotificationRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGApprovalRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGRequest;
import com.dpw.runner.shipment.services.dto.response.AdditionalDetailResponse;
import com.dpw.runner.shipment.services.dto.response.AllShipmentCountResponse;
import com.dpw.runner.shipment.services.dto.response.AttachListShipmentResponse;
import com.dpw.runner.shipment.services.dto.response.CarrierDetailResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationListResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.DateTimeChangeLogResponse;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.dto.response.FieldClassDto;
import com.dpw.runner.shipment.services.dto.response.GenerateCustomHblResponse;
import com.dpw.runner.shipment.services.dto.response.HblCheckResponse;
import com.dpw.runner.shipment.services.dto.response.LatestCargoDeliveryInfo;
import com.dpw.runner.shipment.services.dto.response.MasterDataDescriptionResponse;
import com.dpw.runner.shipment.services.dto.response.MeasurementBasisResponse;
import com.dpw.runner.shipment.services.dto.response.NotesResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentExcelExportResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentListResponse;
import com.dpw.runner.shipment.services.dto.response.TriangulationPartnerResponse;
import com.dpw.runner.shipment.services.dto.response.UpstreamDateUpdateResponse;
import com.dpw.runner.shipment.services.dto.response.billing.InvoicePostingValidationResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingShipmentActionsResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Container;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceLiteContainerResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceLiteContainerResponse.LiteContainer;
import com.dpw.runner.shipment.services.dto.v1.request.AddressTranslationRequest;
import com.dpw.runner.shipment.services.dto.v1.request.PartiesOrgAddressRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TIContainerListRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TIListRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TaskCreateRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TaskStatusUpdateRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TaskStatusUpdateRequest.EntityDetails;
import com.dpw.runner.shipment.services.dto.v1.request.WayBillNumberFilterRequest;
import com.dpw.runner.shipment.services.dto.v1.response.AddressDataV1;
import com.dpw.runner.shipment.services.dto.v1.response.CheckActiveInvoiceResponse;
import com.dpw.runner.shipment.services.dto.v1.response.CreditLimitResponse;
import com.dpw.runner.shipment.services.dto.v1.response.GuidsListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.OrgDataV1;
import com.dpw.runner.shipment.services.dto.v1.response.TIContainerResponse;
import com.dpw.runner.shipment.services.dto.v1.response.TIResponse;
import com.dpw.runner.shipment.services.dto.v1.response.TaskCreateResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.DateTimeChangeLog;
import com.dpw.runner.shipment.services.entity.DpsEvent;
import com.dpw.runner.shipment.services.entity.ELDetails;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.MblDuplicatedLog;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.entity.OceanDGRequestLog;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.QuartzJobInfo;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentOrder;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import com.dpw.runner.shipment.services.entity.TenantProducts;
import com.dpw.runner.shipment.services.entity.TriangulationPartner;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.exception.exceptions.billing.BillingException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.MasterDataHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.mapper.CarrierDetailsMapper;
import com.dpw.runner.shipment.services.mapper.ShipmentDetailsMapper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.projection.ConsolidationDetailsProjection;
import com.dpw.runner.shipment.services.projection.ShipmentDetailsProjection;
import com.dpw.runner.shipment.services.service.interfaces.IApplicationConfigService;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.interfaces.IDateTimeChangeLogService;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.dpw.runner.shipment.services.service.interfaces.IHblService;
import com.dpw.runner.shipment.services.service.interfaces.ILogsHistoryService;
import com.dpw.runner.shipment.services.service.interfaces.INetworkTransferService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.service.interfaces.IQuartzJobInfoService;
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.syncing.AuditLogsSyncRequest;
import com.dpw.runner.shipment.services.syncing.Entity.AuditLogRequestV2;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.impl.SyncEntityConversionService;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IHblSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingsSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.BookingIntegrationsUtility;
import com.dpw.runner.shipment.services.utils.CSVParsingUtil;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.ExcludePermissions;
import com.dpw.runner.shipment.services.utils.FieldUtils;
import com.dpw.runner.shipment.services.utils.GetNextNumberHelper;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import com.dpw.runner.shipment.services.utils.ProductIdentifierUtility;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.UnitConversionUtility;
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;
import com.nimbusds.jose.util.Pair;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;
import org.apache.http.auth.AuthenticationException;
import org.apache.poi.ss.formula.functions.T;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.jetbrains.annotations.Nullable;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.modelmapper.ModelMapper;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.dao.InvalidDataAccessApiUsageException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;
import org.springframework.util.CollectionUtils;
import org.springframework.web.client.RestTemplate;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ShipmentService implements IShipmentService {

    @Autowired
    ExecutorService executorService;

    @Autowired
    @Qualifier("executorServiceMasterData")
    ExecutorService executorServiceMasterData;
    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private IApplicationConfigService applicationConfigService;

    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private ShipmentDetailsMapper shipmentDetailsMapper;
    @Autowired
    private CarrierDetailsMapper carrierDetailsMapper;

    @Autowired
    private CSVParsingUtil<ShipmentDetails> parser;

    @Autowired
    private INotificationService notificationService;

    @Autowired
    private IShipmentDao shipmentDao;
    @Autowired
    private IPartiesDao partiesDao;

    @Autowired
    private IAdditionalDetailDao additionalDetailDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private MasterDataHelper masterDataHelper;

    @Autowired
    private IPackingDao packingDao;

    @Autowired
    private IPackingsSync packingsSync;

    @Autowired
    private IPackingService packingService;

    @Autowired
    private IContainerService containerService;

    @Autowired
    private ILogsHistoryService logsHistoryService;

    @Autowired
    private ITruckDriverDetailsDao truckDriverDetailsDao;
    @Autowired
    private IBookingCarriageDao bookingCarriageDao;

    @Autowired
    private IELDetailsDao elDetailsDao;

    @Autowired
    private IEventDao eventDao;

    @Autowired
    private INotesDao notesDao;

    @Autowired
    private IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;

    @Autowired
    private IReferenceNumbersDao referenceNumbersDao;

    @Autowired
    private IRoutingsDao routingsDao;

    @Autowired
    private IRoutingsService routingsService;

    @Autowired
    private IServiceDetailsDao serviceDetailsDao;

    @Autowired
    private IContainerDao containerDao;

    @Autowired
    private TransactionTemplate transactionTemplate;

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    UserContext userContext;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private DependentServiceHelper dependentServiceHelper;

    @Autowired
    private SyncEntityConversionService syncEntityConversionService;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private IConsolidationService consolidationService;

    @Autowired
    private IEventService eventService;

    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    private IAwbDao awbDao;
    @Autowired
    private IHblDao hblDao;

    @Autowired
    private MasterDataKeyUtils masterDataKeyUtils;

    @Autowired
    private IAuditLogService auditLogService;
    @Autowired
    IShipmentSync shipmentSync;
    @Autowired
    IHblService hblService;
    @Autowired
    IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    private IConsolidationSync consolidationSync;

    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private ISequenceIncrementorDao sequenceIncrementorDao;

    @Autowired
    private IOrderManagementAdapter orderManagementAdapter;

    @Value("${shipmentsKafka.queue}")
    private String senderQueue;

    @Autowired
    private KafkaProducer producer;

    @Autowired
    private ITrackingServiceAdapter trackingServiceAdapter;

    @Autowired
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;

    @Autowired
    private GetNextNumberHelper getNextNumberHelper;

    @Autowired
    private IHblSync hblSync;

    @Autowired
    private V1ServiceUtil v1ServiceUtil;

    @Autowired
    private ProductIdentifierUtility productEngine;

    @Autowired
    private IDateTimeChangeLogService dateTimeChangeLogService;

    private SecureRandom rnd = new SecureRandom();

    @Autowired
    private PartialFetchUtils partialFetchUtils;

    @Autowired
    private RestTemplate restTemplate;

    @Autowired
    private BillingServiceAdapter billingServiceAdapter;

    @Autowired
    private IShipmentOrderDao shipmentOrderDao;

    @Autowired
    private ICarrierDetailsDao carrierDetailsDao;

    @Autowired
    private INetworkTransferService networkTransferService;

    @Autowired
    private INetworkTransferDao networkTransferDao;

    @Autowired
    private IQuartzJobInfoService quartzJobInfoService;

    @Autowired
    private IQuartzJobInfoDao quartzJobInfoDao;

    @Autowired
    private IDpsEventService dpsEventService;

    @Autowired
    private ICommonErrorLogsDao commonErrorLogsDao;

    @Autowired
    private INotificationDao notificationDao;

    @Autowired
    private IDocumentManagerService documentManagerService;

    @Value("${include.master.data}")
    private Boolean includeMasterData;

    public static final String CONSOLIDATION_ID = "consolidationId";
    public static final String TEMPLATE_NOT_FOUND_MESSAGE = "Template not found, please inform the region users manually";

    @Autowired @Lazy
    private BookingIntegrationsUtility bookingIntegrationsUtility;
    private List<String> TRANSPORT_MODES = Arrays.asList("SEA", "ROAD", "RAIL", "AIR");
    private List<String> SHIPMENT_TYPE = Arrays.asList("FCL", "LCL");
    private List<String> DIRECTIONS = Arrays.asList("IMP", "EXP");
    private List<String> SOURCE = Arrays.asList("API", "Runner", "Logistics");

    public static final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry(Constants.CLIENT_ORG_CODE, RunnerEntityMapping.builder().tableName(Constants.CLIENT).dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build()),
            Map.entry(Constants.CONSIGNER_ORG_CODE, RunnerEntityMapping.builder().tableName(Constants.CONSIGNER).dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build()),
            Map.entry(Constants.CONSIGNEE_ORG_CODE, RunnerEntityMapping.builder().tableName(Constants.CONSIGNEE).dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build()),
            Map.entry(Constants.CLIENT_ADDRESS_CODE, RunnerEntityMapping.builder().tableName(Constants.CLIENT).dataType(Integer.class).fieldName(Constants.ADDRESS_CODE).isContainsText(true).build()),
            Map.entry(Constants.CONSIGNER_ADDRESS_CODE, RunnerEntityMapping.builder().tableName(Constants.CONSIGNER).dataType(String.class).fieldName(Constants.ADDRESS_CODE).isContainsText(true).build()),
            Map.entry(Constants.CONSIGNEE_ADDRESS_CODE, RunnerEntityMapping.builder().tableName(Constants.CONSIGNEE).dataType(String.class).fieldName(Constants.ADDRESS_CODE).isContainsText(true).build()),
            Map.entry("houseBill", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("houseBill").isContainsText(true).build()),
            Map.entry("houseBillType", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("houseBillType").isContainsText(true).build()),
            Map.entry(Constants.TRANSPORT_MODE, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.TRANSPORT_MODE).isContainsText(true).build()),
            Map.entry("releaseType", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("releaseType").isContainsText(true).build()),
            Map.entry("deliveryMode", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("deliveryMode").isContainsText(true).build()),
            Map.entry(Constants.DIRECTION, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.DIRECTION).isContainsText(true).build()),
            Map.entry("shipmentType", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("shipmentType").isContainsText(true).build()),
            Map.entry(Constants.STATUS, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Integer.class).fieldName(Constants.STATUS).build()),
            Map.entry("guid", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(UUID.class).fieldName("guid").build()),
            Map.entry("source", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("source").isContainsText(true).build()),
            Map.entry(Constants.JOB_TYPE, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.JOB_TYPE).isContainsText(true).build()),
            Map.entry("createdBy", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("createdBy").isContainsText(true).build()),
            Map.entry("serviceType", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("serviceType").isContainsText(true).build()),
            Map.entry("masterBill", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("masterBill").isContainsText(true).build()),
            Map.entry("bookingReference", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("bookingReference").isContainsText(true).build()),
            Map.entry("consolRef", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("consolRef").isContainsText(true).build()),
            Map.entry("salesAgent", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Long.class).fieldName("salesAgent").build()),
            Map.entry("paymentTerms", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("paymentTerms").isContainsText(true).build()),
            Map.entry("incoterms", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("incoterms").isContainsText(true).build()),
            Map.entry(Constants.SHIPMENT_ID, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.SHIPMENT_ID).isContainsText(true).build()),
            Map.entry("isDomestic", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Boolean.class).fieldName("isDomestic").build()),
            Map.entry("assignedTo", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Integer.class).fieldName("assignedTo").build()),
            Map.entry("additionalTerms", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("additionalTerms").isContainsText(true).build()),
            Map.entry("goodsDescription", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("goodsDescription").isContainsText(true).build()),
            Map.entry("createdAt", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(LocalDateTime.class).fieldName("createdAt").build()),
            Map.entry("updatedAt", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(LocalDateTime.class).fieldName("updatedAt").build()),
            Map.entry("deliveryEstimated", RunnerEntityMapping.builder().tableName(Constants.DELIVERY_DETAILS).dataType(LocalDateTime.class).fieldName("estimatedPickupOrDelivery").build()),
            Map.entry("deliveryActual", RunnerEntityMapping.builder().tableName(Constants.DELIVERY_DETAILS).dataType(LocalDateTime.class).fieldName("actualPickupOrDelivery").build()),
            Map.entry("deliveryRequiredBy", RunnerEntityMapping.builder().tableName(Constants.DELIVERY_DETAILS).dataType(LocalDateTime.class).fieldName("requiredBy").build()),
            Map.entry("pickupEstimated", RunnerEntityMapping.builder().tableName(Constants.PICKUP_DETAILS).dataType(LocalDateTime.class).fieldName("estimatedPickupOrDelivery").build()),
            Map.entry("pickupActual", RunnerEntityMapping.builder().tableName(Constants.PICKUP_DETAILS).dataType(LocalDateTime.class).fieldName("actualPickupOrDelivery").build()),
            Map.entry("pickupRequiredBy", RunnerEntityMapping.builder().tableName(Constants.PICKUP_DETAILS).dataType(LocalDateTime.class).fieldName("requiredBy").build()),
            Map.entry("screeningStatus", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("screeningStatus").build()),
            Map.entry(ShipmentConstants.PAID_PLACE, RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(Long.class).fieldName(ShipmentConstants.PAID_PLACE).build()),
            Map.entry(ShipmentConstants.PLACE_OF_ISSUE, RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(Long.class).fieldName(ShipmentConstants.PLACE_OF_ISSUE).build()),
            Map.entry("dateOfIssue", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(LocalDateTime.class).fieldName("dateOfIssue").build()),
            Map.entry("dateOfReceipt", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(LocalDateTime.class).fieldName("dateOfReceipt").build()),
            Map.entry("goodsCo", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("goodsCo").build()),
            Map.entry("BOEDate", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(LocalDateTime.class).fieldName("BOEDate").build()),
            Map.entry("boeNumber", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("BOENumber").isContainsText(true).build()),
            Map.entry(Constants.SHIPPING_LINE, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.SHIPPING_LINE).isContainsText(true).build()),
            Map.entry(Constants.VESSEL, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.VESSEL).build()),
            Map.entry(Constants.VOYAGE, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.VOYAGE).build()),
            Map.entry(ShipmentConstants.ORIGIN, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(ShipmentConstants.ORIGIN).build()),
            Map.entry(ShipmentConstants.DESTINATION, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(ShipmentConstants.DESTINATION).build()),
            Map.entry(Constants.ORIGIN_PORT, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.ORIGIN_PORT).build()),
            Map.entry(Constants.DESTINATION_PORT, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.DESTINATION_PORT).build()),
            Map.entry("originLocCode", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("originLocCode").build()),
            Map.entry("destinationLocCode", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("destinationLocCode").build()),
            Map.entry(ShipmentConstants.ORIGIN_PORT_LOC_CODE, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(ShipmentConstants.ORIGIN_PORT_LOC_CODE).build()),
            Map.entry(ShipmentConstants.DESTINATION_PORT_LOC_CODE, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(ShipmentConstants.DESTINATION_PORT_LOC_CODE).build()),
            Map.entry("eta", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).fieldName("eta").build()),
            Map.entry("etd", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).fieldName("etd").build()),
            Map.entry("ata", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).fieldName("ata").build()),
            Map.entry("atd", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).fieldName("atd").build()),
            Map.entry("weight", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(BigDecimal.class).fieldName("weight").build()),
            Map.entry("weightUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("weightUnit").build()),
            Map.entry("volume", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(BigDecimal.class).fieldName("volume").build()),
            Map.entry("volumeUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("volumeUnit").build()),
            Map.entry("volumetricWeight", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(BigDecimal.class).fieldName("volumetricWeight").build()),
            Map.entry("volumetricWeightUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("volumetricWeightUnit").build()),
            Map.entry("chargable", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(BigDecimal.class).fieldName("chargable").build()),
            Map.entry("chargeableUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("chargeableUnit").build()),
            Map.entry("netWeight", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(BigDecimal.class).fieldName("netWeight").build()),
            Map.entry("netWeightUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("netWeightUnit").build()),
            Map.entry("noOfPacks", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Integer.class).fieldName("noOfPacks").build()),
            Map.entry("packsUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("packsUnit").build()),
            Map.entry("innerPacks", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Integer.class).fieldName("innerPacks").build()),
            Map.entry("innerPackUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("innerPackUnit").build()),
            Map.entry("jobStatus", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("jobStatus").build()),
            Map.entry("containerNumber", RunnerEntityMapping.builder().tableName(Constants.CONTAINERS_LIST).dataType(String.class).fieldName("containerNumber").build()),
            Map.entry("containerCode", RunnerEntityMapping.builder().tableName(Constants.CONTAINERS_LIST).dataType(String.class).fieldName("containerCode").build()),
            Map.entry("id", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Long.class).fieldName("id").build()),
            Map.entry("consolidationNumber", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_LIST).dataType(String.class).fieldName("consolidationNumber").build()),
            Map.entry(Constants.ORDER_NUMBER, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.ORDER_NUMBER).build()),
            Map.entry(Constants.ORDER_MANAGEMENT_NUMBER, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.ORDER_MANAGEMENT_NUMBER).build()),
            Map.entry("referenceNumber", RunnerEntityMapping.builder().tableName("referenceNumbersList").dataType(String.class).fieldName("referenceNumber").build()),
            Map.entry("activityType", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("activityType").build()),
            Map.entry("goodsCO", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("goodsCO").build()),
            Map.entry("route", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("route").build()),
            Map.entry("cargoFinanceBooking", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Boolean.class).fieldName("cargoFinanceBooking").build()),
            Map.entry("isCmsHBLSent", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(Boolean.class).fieldName("isCmsHBLSent").build()),
            Map.entry(Constants.ORDER_MANAGEMENT_ID, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.ORDER_MANAGEMENT_ID).isContainsText(true).build()),
            Map.entry(Constants.FLIGHT_NUMBER, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.FLIGHT_NUMBER).build()),
            Map.entry(CONSOLIDATION_ID, RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_LIST).dataType(Long.class).fieldName("id").build()),
            Map.entry("voyageOrFlightNumber", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("voyageOrFlightNumber").build()),
            Map.entry("shipperRef", RunnerEntityMapping.builder().tableName(Constants.PICKUP_DETAILS).dataType(String.class).fieldName("shipperRef").build()),
            Map.entry(CONTAINS_HAZARDOUS, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Boolean.class).build()),
            Map.entry("shipmentPackStatus", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(ShipmentPackStatus.class).build()),
            Map.entry(Constants.TENANT_ID, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Integer.class).fieldName(Constants.TENANT_ID).build()),
            Map.entry("cargoReadyDate", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(LocalDateTime.class).fieldName("cargoReadyDate").build()),
            Map.entry("cargoDeliveryDate", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(LocalDateTime.class).fieldName("cargoDeliveryDate").build()),
            Map.entry("requestedOn", RunnerEntityMapping.builder().tableName("consoleShipmentMappings").dataType(LocalDateTime.class).fieldName(CREATED_AT).build()),
            Map.entry("sourceGuid", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(UUID.class).fieldName("sourceGuid").build()),
            Map.entry("routingPol", RunnerEntityMapping.builder().tableName(Constants.ROUTING_LIST).dataType(String.class).fieldName("pol").build()),
            Map.entry("routingPolCode", RunnerEntityMapping.builder().tableName(Constants.ROUTING_LIST).dataType(String.class).fieldName(ShipmentConstants.ORIGIN_PORT_LOC_CODE).build()),
            Map.entry("routingPod", RunnerEntityMapping.builder().tableName(Constants.ROUTING_LIST).dataType(String.class).fieldName("pod").build()),
            Map.entry("routingPodCode", RunnerEntityMapping.builder().tableName(Constants.ROUTING_LIST).dataType(String.class).fieldName("destinationPortLocCode").build()),
            Map.entry("routingCarriage", RunnerEntityMapping.builder().tableName(Constants.ROUTING_LIST).dataType(RoutingCarriage.class).fieldName("carriage").build()),
            Map.entry("fileStatus", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(FileStatus.class).fieldName("fileStatus").build()),
            Map.entry("isFrob", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Boolean.class).fieldName("isFrob").build())
    );

    @Override
    @Transactional
    public List<ShipmentDetails> createTestShipment(Integer count) throws RunnerException {
        List<ShipmentDetails> response = new ArrayList<>();
        /**
         * BL details
         * Measurements
         * carrier
         * pickup
         * Delivery* *
         * Shipment details
         * Parties*
         * * * * * * *
         * * * */

        for (int i = 0; i < count; i++) {

            ShipmentDetails shipmentDetail = createShipmentData();
            /**
             * Carrier Details*
             */

            shipmentDetail = shipmentDao.save(shipmentDetail, false);
            dependentServiceHelper.pushShipmentDataToDependentService(shipmentDetail, true, false, shipmentDetail.getContainersList());
        }

        return response;
    }

    @Override
    public ResponseEntity<IRunnerResponse> fetchShipments(CommonRequestModel commonRequestModel) {
        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
        request.setIncludeTbls(Arrays.asList(Constants.ADDITIONAL_DETAILS, Constants.CLIENT, Constants.CONSIGNER, Constants.CONSIGNEE, Constants.CARRIER_DETAILS, Constants.PICKUP_DETAILS, Constants.DELIVERY_DETAILS));
        Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
        return ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements());
    }

    private List<IRunnerResponse> convertEntityToDtoListSimplified(List<ShipmentDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(shipmentDetails -> {
            ShipmentListResponse response = modelMapper.map(shipmentDetails, ShipmentListResponse.class);
            if (shipmentDetails.getStatus() != null && shipmentDetails.getStatus() < ShipmentStatus.values().length)
                response.setShipmentStatus(ShipmentStatus.values()[shipmentDetails.getStatus()].toString());
            responseList.add(response);
        });
        return responseList;
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ShipmentDetails> lst) {
        return convertEntityListToDtoList(lst, false);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ShipmentDetails> lst, boolean getMasterData) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        List<Long> shipmentIdList = lst.stream().map(ShipmentDetails::getId).toList();
        var map = consoleShipmentMappingDao.pendingStateCountBasedOnShipmentId(shipmentIdList, ShipmentRequestedType.SHIPMENT_PULL_REQUESTED.ordinal());
        var notificationMap = notificationDao.pendingNotificationCountBasedOnEntityIdsAndEntityType(shipmentIdList, SHIPMENT);
        List<ShipmentListResponse> shipmentListResponses = ShipmentMapper.INSTANCE.toShipmentListResponses(lst);
        shipmentListResponses.forEach(response -> {
            setEventData(response);
            setDpsData(response);
            setShipperReferenceNumber(response);
            if (response.getStatus() != null && response.getStatus() < ShipmentStatus.values().length)
                response.setShipmentStatus(ShipmentStatus.values()[response.getStatus()].toString());
            int pendingCount = map.getOrDefault(response.getId(), 0) + notificationMap.getOrDefault(response.getId(), 0);
            response.setPendingActionCount((pendingCount == 0) ? null : pendingCount);
            if(ObjectUtils.isNotEmpty(response.getShipmentOrders()))
                response.setOrdersCount(response.getShipmentOrders().size());
            responseList.add(response);
        });
        this.getMasterDataForList(lst, responseList, getMasterData, true);
        return responseList;
    }

    /**
     * Sets the DPS and CGS status on the given {@link ShipmentListResponse} based on associated {@link DpsEvent}s retrieved using the shipment GUID.
     * <p>
     * - DPS status is set to "On Hold" if any event is of type HOLD and state is either PER_BLOCKED or HOLD. - CGS status is set based on the presence of HOLD or WARNING type
     * events.
     *
     * @param response the shipment response object on which statuses are to be set
     */
    public void setDpsData(ShipmentListResponse response) {
        String guid = response.getGuid().toString();

        // Fetch all DPS events for the given shipment GUID
        List<DpsEvent> dpsEvents = dpsEventService.findDpsEventByGuidAndExecutionState(guid);
        log.info("Fetched {} DPS events for shipment GUID: {}", dpsEvents.size(), guid);

        // Determine DPS status based on HOLD events with PER_BLOCKED or HOLD state
        boolean isOnDpsHold = dpsEvents.stream().anyMatch(event ->
                DpsWorkflowType.HOLD.equals(event.getWorkflowType()) &&
                        (DpsWorkflowState.PER_BLOCKED.equals(event.getState()) || DpsWorkflowState.HOLD.equals(event.getState()))
        );

        if (isOnDpsHold) {
            response.setDpsStatus("On Hold");
            log.info("DPS status set to 'On Hold' for shipment GUID: {}", guid);
        } else {
            response.setDpsStatus("No Hold");
            log.info("DPS status set to 'No Hold' for shipment GUID: {}", guid);
        }

        // Determine CGS status based on HOLD or WARNING events
        boolean hasHoldEvent = dpsEvents.stream().anyMatch(event ->
                DpsWorkflowType.HOLD.equals(event.getWorkflowType())
        );
        boolean hasWarningEvent = dpsEvents.stream().anyMatch(event ->
                DpsWorkflowType.WARNING.equals(event.getWorkflowType())
        );

        if (hasHoldEvent) {
            response.setCgsStatus("On Hold");
            log.info("CGS status set to 'On Hold' for shipment GUID: {}", guid);
        } else if (hasWarningEvent) {
            response.setCgsStatus("On Warning");
            log.info("CGS status set to 'On Warning' for shipment GUID: {}", guid);
        } else {
            response.setCgsStatus("No Hold");
            log.info("CGS status set to 'No Hold' for shipment GUID: {}", guid);
        }
    }

    private List<IRunnerResponse> convertEntityListToDtoListForAttachListShipment(List<ShipmentDetails> lst, boolean getMasterData) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        List<Long> shipmentIdList = lst.stream().map(ShipmentDetails::getId).toList();
        var map = consoleShipmentMappingDao.pendingStateCountBasedOnShipmentId(shipmentIdList, ShipmentRequestedType.SHIPMENT_PULL_REQUESTED.ordinal());
        var notificationMap = notificationDao.pendingNotificationCountBasedOnEntityIdsAndEntityType(shipmentIdList, SHIPMENT);
        List<AttachListShipmentResponse> attachListShipmentResponse = AttachListShipmentMapper.INSTANCE.toAttachListShipmentResponse(lst);
        attachListShipmentResponse.forEach(response -> {
            if (response.getStatus() != null && response.getStatus() < ShipmentStatus.values().length)
                response.setShipmentStatus(ShipmentStatus.values()[response.getStatus()].toString());
            int pendingCount = map.getOrDefault(response.getId(), 0) + notificationMap.getOrDefault(response.getId(), 0);
            response.setPendingActionCount((pendingCount == 0) ? null : pendingCount);
            responseList.add(response);
        });
        this.getMasterDataForList(lst, responseList, getMasterData, true);
        return responseList;
    }

    private void getMasterDataForList(List<ShipmentDetails> lst, List<IRunnerResponse> responseList, boolean getMasterData, boolean includeTenantData) {
        if(getMasterData || Boolean.TRUE.equals(includeMasterData)) {
            try {
                double startTime = System.currentTimeMillis();
                var locationDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setLocationData(responseList, EntityTransferConstants.LOCATION_SERVICE_GUID)), executorServiceMasterData);
                var containerDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setContainerTeuData(lst, responseList)), executorServiceMasterData);
                CompletableFuture<Void> billDataFuture = CompletableFuture.completedFuture(null);
                if(responseList!=null && !(responseList.get(0) instanceof AttachListShipmentResponse)){
                    billDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchBillDataForShipments(lst, responseList)), executorServiceMasterData);
                }
                var vesselDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchVesselForList(responseList)), executorServiceMasterData);
                CompletableFuture<Void> tenantDataFuture = CompletableFuture.completedFuture(null);
                if (Boolean.TRUE.equals(includeTenantData))
                    tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchTenantIdForList(responseList)), executorServiceMasterData);
                CompletableFuture.allOf(locationDataFuture, containerDataFuture, billDataFuture, vesselDataFuture, tenantDataFuture).join();
                log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.SHIPMENT_LIST_MASTER_DATA, (System.currentTimeMillis() - startTime) , LoggerHelper.getRequestIdFromMDC());
            }
            catch (Exception ex) {
                log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_LIST, ex.getLocalizedMessage());
            }
        }
    }

    private List<IRunnerResponse> convertEntityListToDtoListForExport(List<ShipmentDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        List<ShipmentExcelExportResponse> shipmentListResponses = ShipmentMapper.INSTANCE.toShipmentExportListResponses(lst);
        shipmentListResponses.forEach(response -> {
            if (response.getStatus() != null && response.getStatus() < ShipmentStatus.values().length)
                response.setShipmentStatus(ShipmentStatus.values()[response.getStatus()].toString());
            responseList.add(jsonHelper.convertValue(response, ShipmentListResponse.class));
        });
        this.getMasterDataForList(lst, responseList, true, false);
        return responseList;
    }

    private void setEventData(ShipmentListResponse response) {
        if (response.getEventsList() != null) {
            for (EventsResponse events : response.getEventsList()) {
                if (StringUtility.isNotEmpty(events.getEventCode())) {
                    if (events.getEventCode().equalsIgnoreCase(EventConstants.INVGNTD)) {
                        response.setInvoiceDate(events.getActual());
                    } else if (events.getEventCode().equalsIgnoreCase(EventConstants.TAXSG)) {
                        response.setTaxDate(events.getActual());
                    } else if (events.getEventCode().equalsIgnoreCase(EventConstants.CSEDI)) {
                        response.setCustomsFilingDate(events.getActual());
                    } else if (events.getEventCode().equalsIgnoreCase(EventConstants.AMSEDI)) {
                        response.setAmsFilingDate(events.getActual());
                    }
                }
            }
        }
    }

    private ShipmentDetails createShipmentData() {
        int random = rnd.nextInt(100);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().direction(DIRECTIONS.get(random % DIRECTIONS.size())).status(1)
                .source(SOURCE.get(random % SOURCE.size())).transportMode(TRANSPORT_MODES.get(random % TRANSPORT_MODES.size())).shipmentType(SHIPMENT_TYPE.get(random % SHIPMENT_TYPE.size()))
                .houseBill(generateString(10)).masterBill(generateString(10)).bookingReference(generateString(10)).consolRef(generateString(10)).paymentTerms(generateString(3))
                .goodsDescription(generateString(10)).additionalTerms(generateString(10))
                .build();
        shipmentDetails.setTenantId(1);
        return shipmentDetails;
    }

    private String generateString(int length) {
        StringBuilder salt = new StringBuilder();
        while (salt.length() < length) {
            salt.append(Constants.SALT_CHARS.charAt(Math.abs(this.rnd.nextInt() * Constants.SALT_CHARS.length()) % Constants.SALT_CHARS.length()));
        }
        return salt.toString();
    }

    public ResponseEntity<IRunnerResponse> createFromBooking(CommonRequestModel commonRequestModel)
    {
        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Shipment Create From Booking with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        ShipmentDetails shipmentDetails = jsonHelper.convertValue(request, ShipmentDetails.class);
        try {
            /*  Populate unloc code for entities */
            var populateUnlocCodeFuture = getPopulateUnlocCodeFuture(shipmentDetails, null);

            if(request.getConsolidationList() != null)
                shipmentDetails.setConsolidationList(new HashSet<>(jsonHelper.convertValueToList(request.getConsolidationList().stream().toList(), ConsolidationDetails.class)));
            if(request.getContainersList() != null)
                shipmentDetails.setContainersList(new HashSet<>(jsonHelper.convertValueToList(request.getContainersList().stream().toList(), Containers.class)));

            populateUnlocCodeFuture.join();

            if(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getIsEntityTransferPrerequisiteEnabled())) {
                if(!commonUtils.checkIfPartyExists(shipmentDetails.getAdditionalDetails().getImportBroker())) {
                    shipmentDetails.getAdditionalDetails().setImportBrokerCountry(commonUtils.getCountryFromUnLocCode(shipmentDetails.getCarrierDetails().getDestinationLocCode()));
                }
                if(!commonUtils.checkIfPartyExists(shipmentDetails.getAdditionalDetails().getExportBroker())) {
                    shipmentDetails.getAdditionalDetails().setExportBrokerCountry(commonUtils.getCountryFromUnLocCode(shipmentDetails.getCarrierDetails().getOriginLocCode()));
                }
            }
            populateOriginDestinationAgentDetailsForBookingShipment(shipmentDetails);
            shipmentDetails = getShipment(shipmentDetails);
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            if(shipmentSettingsDetails.getAutoEventCreate() != null && shipmentSettingsDetails.getAutoEventCreate())
                autoGenerateCreateEvent(shipmentDetails);
            autoGenerateEvents(shipmentDetails);
            Long shipmentId = shipmentDetails.getId();
            List<Packing> updatedPackings = getAndSetPackings(request, shipmentId, shipmentDetails);
            List<RoutingsRequest> routingsRequest = request.getRoutingsList();
            if (ObjectUtils.isNotEmpty(routingsRequest))
                shipmentDetails.setRoutingsList(routingsDao.saveEntityFromShipment(shipmentDetails.getRoutingsList(), shipmentId));

            List<ReferenceNumbersRequest> referenceNumbersRequest = request.getReferenceNumbersList();
            if (ObjectUtils.isNotEmpty(referenceNumbersRequest))
                shipmentDetails.setReferenceNumbersList(referenceNumbersDao.saveEntityFromShipment(jsonHelper.convertValueToList(referenceNumbersRequest, ReferenceNumbers.class), shipmentId));

            List<ShipmentOrderRequest> shipmentOrderRequestList = request.getShipmentOrders();
            if(ObjectUtils.isNotEmpty(shipmentOrderRequestList)) {
                shipmentDetails.setShipmentOrders(shipmentOrderDao.updateEntityFromShipment(jsonHelper.convertValueToList(shipmentOrderRequestList, ShipmentOrder.class), shipmentId));
            }

            checkContainerAssignedForHbl(shipmentDetails, updatedPackings);

            List<NotesRequest> notesRequest = getNotesRequests(request, shipmentId);
            dependentServiceHelper.pushShipmentDataToDependentService(shipmentDetails, true, false, null);
            setShipmentFromBooking(shipmentDetails, notesRequest);

            auditLogService.addAuditLog(
                AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                        .newData(shipmentDetails)
                        .prevData(null)
                        .parent(ShipmentDetails.class.getSimpleName())
                        .parentId(shipmentDetails.getId())
                        .operation(DBOperationType.CREATE.name()).build()
            );
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ValidationException(e.getMessage());
        }
        ShipmentDetailsResponse shipmentDetailsResponse = jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);
        CompletableFuture.runAsync(masterDataUtils.withMdc(() -> addFilesFromBookingToShipment(shipmentDetailsResponse.getGuid().toString(), shipmentDetailsResponse.getCustomerBookingGuid().toString())), executorService);
        return ResponseHelper.buildSuccessResponse(shipmentDetailsResponse);
    }

    private void checkContainerAssignedForHbl(ShipmentDetails shipmentDetails, List<Packing> updatedPackings) {
        if(shipmentDetails.getContainersList() != null && !shipmentDetails.getContainersList().isEmpty()) {
            hblService.checkAllContainerAssigned(shipmentDetails, shipmentDetails.getContainersList(), updatedPackings);
        }
    }

    private List<NotesRequest> getNotesRequests(ShipmentRequest request, Long shipmentId) {
        List<NotesRequest> notesRequest = request.getNotesList();
        if (notesRequest != null) {
            for(NotesRequest req : notesRequest) {
                req.setEntityId(shipmentId);
            }
        }
        if (notesRequest != null) {
            for(NotesRequest req : notesRequest) {
                notesDao.save(jsonHelper.convertValue(req, Notes.class));
            }
        }
        return notesRequest;
    }

    public DocumentManagerResponse<T> addFilesFromBookingToShipment(String shipmentGuid, String bookingGuid) {
        try {
            List<DocumentManagerUpdateFileEntitiesRequest.UpdateFileRequest> updateFileRequests = new ArrayList<>();
            updateFileRequests.add(DocumentManagerUpdateFileEntitiesRequest.UpdateFileRequest.builder()
                    .source(DocumentManagerUpdateFileEntitiesRequest.EntityData.builder()
                            .entityKey(bookingGuid)
                            .entityType(BOOKINGS_WITH_SQ_BRACKETS)
                            .build())
                    .entitiesToAttach(List.of(DocumentManagerUpdateFileEntitiesRequest.EntityData.builder()
                            .entityKey(shipmentGuid)
                            .entityType(SHIPMENTS_WITH_SQ_BRACKETS)
                            .build()))
                    .build());

            return documentManagerService.updateFileEntities(DocumentManagerUpdateFileEntitiesRequest.builder()
                    .filesToUpdate(updateFileRequests)
                    .build());
        } catch (Exception ex) {
            log.error("CR-ID {} || Error in addFilesFromBookingToShipment: {} with Shipment Guid as: {}",
                    LoggerHelper.getRequestIdFromMDC(), ex.getLocalizedMessage(), shipmentGuid);
        }
        return null;
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();
        this.setColoadingStation(request);
        ShipmentDetailsResponse shipmentDetailsResponse = this.createShipment(request, false, false);

        return ResponseHelper.buildSuccessResponse(shipmentDetailsResponse);
    }

    private ShipmentDetailsResponse createShipment(ShipmentRequest request, boolean includeGuid, boolean isFromET) {
        if (request == null) {
            log.error("Request is null for Shipment Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        ShipmentDetails shipmentDetails = includeGuid ? jsonHelper.convertValue(request, ShipmentDetails.class) : jsonHelper.convertCreateValue(request, ShipmentDetails.class);
        if(request.getConsolidationList() != null)
            shipmentDetails.setConsolidationList(new HashSet<>(jsonHelper.convertValueToList(request.getConsolidationList().stream().toList(), ConsolidationDetails.class)));

        try {
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            List<Long> removedConsolIds = new ArrayList<>();
            MutableBoolean isNewConsolAttached = new MutableBoolean(false);


            boolean syncConsole = beforeSave(shipmentDetails, null, true, request, shipmentSettingsDetails, removedConsolIds, isNewConsolAttached, includeGuid);

            shipmentDetails = getShipment(shipmentDetails);
            Long shipmentId = shipmentDetails.getId();


            if(shipmentDetails.getContainersList() != null && !shipmentDetails.getContainersList().isEmpty())
            {
                for (Containers container: shipmentDetails.getContainersList()) {
                    addAuditLogContainers(container, null, shipmentId, DBOperationType.CREATE.name());
                }
            }

            afterSave(shipmentDetails, null, true, request, shipmentSettingsDetails, syncConsole, removedConsolIds, isNewConsolAttached, includeGuid, isFromET);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(shipmentDetails)
                            .prevData(null)
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(shipmentDetails.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

            ShipmentDetails finalShipmentDetails1 = shipmentDetails;
            String entityPayload = jsonHelper.convertToJson(finalShipmentDetails1);
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.createLogHistoryForShipment(entityPayload, finalShipmentDetails1.getId(), finalShipmentDetails1.getGuid())), executorService);
        } catch (Exception e) {
            log.error("Error occurred due to: " + e.getStackTrace());
            log.error(e.getMessage());
            throw new ValidationException(e.getMessage());
        }
        return jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);
    }

    private void addAuditLogContainers(Containers container, Containers prev, Long shipmentId, String operationName) throws RunnerException {
        try {
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                    .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(container)
                            .prevData(prev)
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(shipmentId)
                            .operation(operationName).build()
            );
        } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException | InvocationTargetException | NoSuchMethodException e) {
            log.error(e.getMessage());
        }
    }

    @Override
    public ShipmentDetailsResponse createShipmentFromEntityTransfer(ShipmentRequest shipmentRequest) {
        return this.createShipment(shipmentRequest, true, true);
    }

    ShipmentDetails getShipment(ShipmentDetails shipmentDetails) throws RunnerException {
        if(shipmentDetails.getShipmentId() == null){
            shipmentDetails.setShipmentId(generateShipmentId(shipmentDetails));
        }
        shipmentDetails = shipmentDao.save(shipmentDetails, false);
        return shipmentDetails;
    }

    @Transactional
    public void createParties(ShipmentDetails shipmentDetails, PartiesRequest partiesRequest) {
        partiesRequest.setEntityId(shipmentDetails.getId());
        partiesRequest.setEntityType("SHIPMENT");
        packingDao.save(objectMapper.convertValue(partiesRequest, Packing.class));
    }


    public Optional<ShipmentDetails> retrieveByIdOrGuid(ShipmentRequest request) throws RunnerException {
        if (request == null) {
            log.error("Request is empty for Shipment update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Optional<ShipmentDetails> oldEntity;

        if(request.getId()!=null){
            long id = request.getId();
            oldEntity=shipmentDao.findById(id);
            if (!oldEntity.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

        }

        else if(request.getGuid()!=null){
            UUID guid = request.getGuid();
            oldEntity= shipmentDao.findByGuid(guid);
            if (!oldEntity.isPresent()) {
                log.debug("Shipment Details is null for GUID {} with Request GUID {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

        }
        else{
            throw new RunnerException("Either Id or Guid is required");

        }
        return oldEntity;
    }
    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> createShipmentInV2(CustomerBookingRequest customerBookingRequest) throws RunnerException
    {
        Set<ConsolidationDetailsRequest> consolidationDetails = new HashSet<>();
        Set<ContainerRequest> containerList = new HashSet<>();
        List<Notes> notes = notesDao.findByEntityIdAndEntityType(customerBookingRequest.getId(), "CustomerBooking");
        boolean isRouteMasterEnabled = commonUtils.getShipmentSettingFromContext().getEnableRouteMaster();
        if(isConsoleCreationNeeded(customerBookingRequest))
        {
            ConsolidationDetailsRequest consolidationDetailsRequest = ConsolidationDetailsRequest.builder().
                    carrierDetails(CarrierDetailRequest.builder()
                            .origin(customerBookingRequest.getCarrierDetails().getOrigin())
                            .destination(customerBookingRequest.getCarrierDetails().getDestination())
                            .shippingLine(customerBookingRequest.getCarrierDetails().getShippingLine())
                            .vessel(customerBookingRequest.getCarrierDetails().getVessel())
                            .voyage(customerBookingRequest.getCarrierDetails().getVoyage())
                            .originPort(customerBookingRequest.getCarrierDetails().getOriginPort())
                            .destinationPort(customerBookingRequest.getCarrierDetails().getDestinationPort())
                            .flightNumber(customerBookingRequest.getCarrierDetails().getFlightNumber())
                            .build()
                    ).
                    consolidationType("STD").
                    transportMode(customerBookingRequest.getTransportType()).
                    containerCategory(customerBookingRequest.getCargoType()).
                    shipmentType(customerBookingRequest.getDirection()).
                    referenceNumber(customerBookingRequest.getBookingNumber()).
                    departureDetails(ArrivalDepartureDetailsRequest.builder().
                            firstForeignPort(customerBookingRequest.getCarrierDetails().getOrigin()).
                            lastForeignPort(customerBookingRequest.getCarrierDetails().getOrigin()).
                            type("Departure").
                            build()
                    ).
                    arrivalDetails(ArrivalDepartureDetailsRequest.builder().
                            firstForeignPort(customerBookingRequest.getCarrierDetails().getDestination()).
                            lastForeignPort(customerBookingRequest.getCarrierDetails().getDestination()).
                            type("Arrival").
                            build()
                    ).
                    containersList(customerBookingRequest.getContainersList()).
                    sourceTenantId(Long.valueOf(UserContext.getUser().TenantId)).
                    build();
            // Set Department in case single department is available
            consolidationDetailsRequest.setDepartment(commonUtils.getAutoPopulateDepartment(
                    consolidationDetailsRequest.getTransportMode(), consolidationDetailsRequest.getShipmentType(), MdmConstants.CONSOLIDATION_MODULE
            ));
            // Generate default routes based on O-D pairs
            if(!Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getIsRunnerV3Enabled()) && Boolean.FALSE.equals(isRouteMasterEnabled)) {
                var routingList = routingsDao.generateDefaultRouting(jsonHelper.convertValue(consolidationDetailsRequest.getCarrierDetails(), CarrierDetails.class), consolidationDetailsRequest.getTransportMode());
                consolidationDetailsRequest.setRoutingsList(commonUtils.convertToList(routingList, RoutingsRequest.class));
            }

            ConsolidationDetailsResponse consolDetailsResponse = consolidationService.createConsolidationForBooking(CommonRequestModel.buildRequest(consolidationDetailsRequest));
            if(consolDetailsResponse != null)
            {
                ConsolidationDetailsRequest consolRequest = jsonHelper.convertValue(consolDetailsResponse, ConsolidationDetailsRequest.class);
                containerList = consolRequest.getContainersList() != null ? new HashSet<>(consolRequest.getContainersList()) : null;
                consolRequest.setContainersList(null);
                consolidationDetails.add(consolRequest);
            }
        }

        List<RoutingsRequest> customerBookingRequestRoutingList = getCustomerBookingRequestRoutingList(customerBookingRequest.getCarrierDetails(), customerBookingRequest.getTransportType());
        ShipmentRequest shipmentRequest = getShipmentRequestFromBooking(customerBookingRequest, consolidationDetails, containerList, isRouteMasterEnabled, customerBookingRequestRoutingList, notes);
        // Set Department in case single department is available
        shipmentRequest.setDepartment(commonUtils.getAutoPopulateDepartment(
                shipmentRequest.getTransportMode(), shipmentRequest.getDirection(), MdmConstants.SHIPMENT_MODULE
        ));
        AutoUpdateWtVolResponse autoUpdateWtVolResponse = calculateShipmentWV(jsonHelper.convertValue(shipmentRequest, AutoUpdateWtVolRequest.class));
        shipmentRequest.setNoOfPacks(getIntFromString(autoUpdateWtVolResponse.getNoOfPacks()));
        shipmentRequest.setPacksUnit(autoUpdateWtVolResponse.getPacksUnit());
        shipmentRequest.setWeight(autoUpdateWtVolResponse.getWeight());
        shipmentRequest.setWeightUnit(autoUpdateWtVolResponse.getWeightUnit());
        shipmentRequest.setVolume(autoUpdateWtVolResponse.getVolume());
        shipmentRequest.setVolumeUnit(autoUpdateWtVolResponse.getVolumeUnit());
        shipmentRequest.setChargable(
                autoUpdateWtVolResponse.getChargable() != null
                        ? autoUpdateWtVolResponse.getChargable().setScale(10, RoundingMode.HALF_UP).stripTrailingZeros()
                        : null
        );
        shipmentRequest.setChargeableUnit(autoUpdateWtVolResponse.getChargeableUnit());
        shipmentRequest.setVolumetricWeight(autoUpdateWtVolResponse.getVolumetricWeight());
        shipmentRequest.setVolumetricWeightUnit(autoUpdateWtVolResponse.getVolumetricWeightUnit());
        shipmentRequest.setNetWeight(autoUpdateWtVolResponse.getNetWeight());
        shipmentRequest.setNetWeightUnit(autoUpdateWtVolResponse.getNetWeightUnit());
        shipmentRequest.setInnerPacks(autoUpdateWtVolResponse.getInnerPacks());
        shipmentRequest.setInnerPackUnit(autoUpdateWtVolResponse.getInnerPackUnit());
        shipmentRequest.setOrderManagementId(customerBookingRequest.getOrderManagementId());
        shipmentRequest.setOrderManagementNumber(customerBookingRequest.getOrderManagementNumber());
        if(!StringUtility.isEmpty(customerBookingRequest.getOrderManagementId())) {
            shipmentRequest.setShipmentOrders(Arrays.asList(ShipmentOrderRequest.builder().orderNumber(customerBookingRequest.getOrderManagementNumber()).orderGuid(UUID.fromString(customerBookingRequest.getOrderManagementId())).build()));
        }

        if(customerBookingRequest.getOrderManagementId()!=null){
            ShipmentDetails shipmentDetails = null;
            shipmentDetails = orderManagementAdapter.getOrderByGuid(customerBookingRequest.getOrderManagementId());

            if(shipmentDetails!=null){
                processShipmentRequestFromDetails(shipmentRequest, shipmentDetails);
            }

        }

        shipmentRequest.setContainsHazardous(customerBookingRequest.getIsDg());
        shipmentRequest.setCustomerBookingGuid(customerBookingRequest.getGuid());
        return this.createFromBooking(CommonRequestModel.buildRequest(shipmentRequest));
    }

    private ShipmentRequest getShipmentRequestFromBooking(CustomerBookingRequest customerBookingRequest, Set<ConsolidationDetailsRequest> consolidationDetails, Set<ContainerRequest> containerList, boolean isRouteMasterEnabled, List<RoutingsRequest> customerBookingRequestRoutingList, List<Notes> notes) {
        return ShipmentRequest.builder().
                carrierDetails(CarrierDetailRequest.builder()
                        .origin(customerBookingRequest.getCarrierDetails().getOrigin())
                        .destination(customerBookingRequest.getCarrierDetails().getDestination())
                        .shippingLine(customerBookingRequest.getCarrierDetails().getShippingLine())
                        .vessel(customerBookingRequest.getCarrierDetails().getVessel())
                        .voyage(customerBookingRequest.getCarrierDetails().getVoyage())
                        .originPort(customerBookingRequest.getCarrierDetails().getOriginPort())
                        .destinationPort(customerBookingRequest.getCarrierDetails().getDestinationPort())
                        .flightNumber(customerBookingRequest.getCarrierDetails().getFlightNumber())
                        .carrierCountry(customerBookingRequest.getCarrierDetails().getCarrierCountry())
                        .minTransitHours(customerBookingRequest.getCarrierDetails().getMinTransitHours())
                        .maxTransitHours(customerBookingRequest.getCarrierDetails().getMaxTransitHours())
                        .carrierAddedFromNpm(customerBookingRequest.getCarrierDetails().getCarrierAddedFromNpm())
                        .build()
                ).
                contractId(customerBookingRequest.getContractId()).
                parentContractId(customerBookingRequest.getParentContractId()).
                contractType(customerBookingRequest.getContractStatus()).
                noOfPacks(customerBookingRequest.getQuantity()).
                packsUnit(customerBookingRequest.getQuantityUnit()).
                weight(customerBookingRequest.getGrossWeight()).
                weightUnit(customerBookingRequest.getGrossWeightUnit()).
                volume(customerBookingRequest.getVolume()).
                volumeUnit(customerBookingRequest.getVolumeUnit()).
                volumetricWeight(customerBookingRequest.getWeightVolume()).
                volumetricWeightUnit(customerBookingRequest.getWeightVolumeUnit()).
                bookingReference(customerBookingRequest.getBookingNumber()).
                bookingCreatedDate(customerBookingRequest.getBookingDate()).
                shipmentCreatedOn(LocalDateTime.now()).
                client(createPartiesRequest(customerBookingRequest.getCustomer(), customerBookingRequest.getClientCountry())).
                consignee(createPartiesRequest(customerBookingRequest.getConsignee(), customerBookingRequest.getConsigneeCountry())).
                consigner(createPartiesRequest(customerBookingRequest.getConsignor(), customerBookingRequest.getConsignorCountry())).
                additionalDetails(AdditionalDetailRequest.builder().
                        notifyParty(createPartiesRequest(customerBookingRequest.getNotifyParty(), customerBookingRequest.getNotifyPartyCountry())).
                        build()
                ).
                shipmentType(customerBookingRequest.getCargoType()).
                transportMode(customerBookingRequest.getTransportType()).
                direction(customerBookingRequest.getDirection()).
                jobType("STD").
                incoterms(customerBookingRequest.getIncoTerms()).
                serviceType(customerBookingRequest.getServiceMode()).
                status(4).
                fmcTlcId(customerBookingRequest.getFmcTlcId()).
                clientCountry(customerBookingRequest.getClientCountry()).
                consignorCountry(customerBookingRequest.getConsignorCountry()).
                consigneeCountry(customerBookingRequest.getConsigneeCountry()).
                notifyPartyCountry(customerBookingRequest.getNotifyPartyCountry()).
                salesBranch(customerBookingRequest.getSalesBranch()).
                primarySalesAgentEmail(customerBookingRequest.getPrimarySalesAgentEmail()).
                secondarySalesAgentEmail(customerBookingRequest.getSecondarySalesAgentEmail()).
                containersList(consolidationDetails != null && !consolidationDetails.isEmpty() ? containerList : null).
                packingList(getPackingListRequest(customerBookingRequest)).
                fileRepoList(customerBookingRequest.getFileRepoList()).
                routingsList(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getIsRunnerV3Enabled()) && Boolean.TRUE.equals(isRouteMasterEnabled) ? null : customerBookingRequestRoutingList).
                consolidationList(isConsoleCreationNeeded(customerBookingRequest) ? consolidationDetails : null).
                referenceNumbersList(createReferenceNumbersList(customerBookingRequest.getReferenceNumbersList())).
                notesList(createNotes(notes)).
                sourceTenantId(Long.valueOf(UserContext.getUser().TenantId)).
                source("API").
                bookingType("ONLINE").
                consolRef(consolidationDetails != null && !consolidationDetails.isEmpty() ? consolidationDetails.iterator().next().getReferenceNumber() : "").
                masterBill(consolidationDetails != null && !consolidationDetails.isEmpty() ? consolidationDetails.iterator().next().getBol() : null).
                freightLocalCurrency(UserContext.getUser().CompanyCurrency).
                currentPartyForQuote(customerBookingRequest.getCurrentPartyForQuote()).
                autoUpdateWtVol(true).
                build();
    }

    private List<PackingRequest> getPackingListRequest(CustomerBookingRequest customerBookingRequest) {
        return customerBookingRequest.getPackingList() != null ? customerBookingRequest.getPackingList().stream().map(obj -> {
            setHeightWidthUnit(obj);
            if(obj.getWeight() != null)
                obj.setWeight(obj.getWeight().multiply(new BigDecimal(obj.getPacks())));
            if(obj.getVolume() != null)
                obj.setVolume(obj.getVolume().multiply(new BigDecimal(obj.getPacks())));
            if(TRANSPORT_MODE_AIR.equalsIgnoreCase(customerBookingRequest.getTransportType())) {
                calculateWeightVolumeForPacks(obj);
            }

            return obj;
        }).collect(Collectors.toList()) : null;
    }

    private void calculateWeightVolumeForPacks(PackingRequest obj) {
        try {
            // Convert Weight to KGs
            if (Objects.nonNull(obj.getWeight())) {
                obj.setWeight(new BigDecimal(convertUnit(MASS, obj.getWeight(), obj.getWeightUnit(), WEIGHT_UNIT_KG).toString()));
                obj.setWeightUnit(Constants.WEIGHT_UNIT_KG);
            }

            // Convert Volume to M3
            if (Objects.nonNull(obj.getVolume())) {
                obj.setVolume(new BigDecimal(convertUnit(VOLUME, obj.getVolume(), obj.getVolumeUnit(), VOLUME_UNIT_M3).toString()));
                obj.setVolumeUnit(Constants.VOLUME_UNIT_M3);

                double factor = Constants.AIR_FACTOR_FOR_VOL_WT;
                BigDecimal wvInKG = obj.getVolume().multiply(BigDecimal.valueOf(factor));
                obj.setVolumeWeight(wvInKG);
                obj.setVolumeWeightUnit(Constants.WEIGHT_UNIT_KG);
            }

            // Calculate chargeable
            if (Objects.nonNull(obj.getWeight()) && Objects.nonNull(obj.getVolumeWeight())) {
                obj.setChargeable(obj.getVolumeWeight().max(obj.getWeight()));
                obj.setChargeableUnit(WEIGHT_UNIT_KG);
            }
        }
        catch (Exception e) {
            log.error("Error while unit conversion for AIR transport mode in shipment packs from booking", e);
        }
    }

    private void setHeightWidthUnit(PackingRequest obj) {
        if(!StringUtility.isEmpty(obj.getLengthUnit()))
        {
            obj.setWidthUnit(obj.getLengthUnit());
            obj.setHeightUnit(obj.getLengthUnit());
        }
    }

    @Override
    public List<RoutingsRequest> getCustomerBookingRequestRoutingList(CarrierDetailRequest carrierDetailRequest, String transportMode) {

        if(ObjectUtils.isEmpty(carrierDetailRequest) || !Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableRouteMaster())) {
            return new ArrayList<>();
        }

        // Get carrier details from the customer booking request
        CarrierDetailRequest carrierDetails = Optional.ofNullable(carrierDetailRequest)
                .orElse(new CarrierDetailRequest());

        List<Routings> routingsList = routingsDao.generateDefaultRouting(jsonHelper.convertValue(carrierDetails, CarrierDetails.class), transportMode);

        return commonUtils.convertToList(routingsList, RoutingsRequest.class);

    }

    @Override
    @Transactional
    public String createShipmentFromBooking(ShipmentRequest shipmentRequest) throws RunnerException{
        Set<ConsolidationDetailsRequest> consolidationDetails = new HashSet<>();
        Set<ContainerRequest> containerList = new HashSet<>();
        if(shipmentRequest.getConsolidationList()!=null){
            for(ConsolidationDetailsRequest consolidationDetailsRequest: shipmentRequest.getConsolidationList()){
                CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationDetailsRequest);
                ConsolidationDetailsResponse consolidationDetailsResponse = consolidationService.createConsolidationForBooking(commonRequestModel);
                ConsolidationDetailsRequest consolRequest = jsonHelper.convertValue(consolidationDetailsResponse, ConsolidationDetailsRequest.class);
                containerList = consolRequest.getContainersList() != null ? new HashSet<>(consolRequest.getContainersList()) : null;
                consolRequest.setContainersList(null);
                consolidationDetails.add(consolRequest);
            }
            if(!consolidationDetails.isEmpty()){
                shipmentRequest.setContainersList(containerList);
                shipmentRequest.setConsolRef(consolidationDetails.iterator().next().getReferenceNumber());
                shipmentRequest.setMasterBill(consolidationDetails.iterator().next().getBol());
                shipmentRequest.setConsolidationList(consolidationDetails);
            }
        }

        AutoUpdateWtVolResponse autoUpdateWtVolResponse = calculateShipmentWV(jsonHelper.convertValue(shipmentRequest, AutoUpdateWtVolRequest.class));
        shipmentRequest.setNoOfPacks(getIntFromString(autoUpdateWtVolResponse.getNoOfPacks()));
        shipmentRequest.setPacksUnit(autoUpdateWtVolResponse.getPacksUnit());
        shipmentRequest.setWeight(autoUpdateWtVolResponse.getWeight());
        shipmentRequest.setWeightUnit(autoUpdateWtVolResponse.getWeightUnit());
        shipmentRequest.setVolume(autoUpdateWtVolResponse.getVolume());
        shipmentRequest.setVolumeUnit(autoUpdateWtVolResponse.getVolumeUnit());
        shipmentRequest.setChargable(autoUpdateWtVolResponse.getChargable());
        shipmentRequest.setChargeableUnit(autoUpdateWtVolResponse.getChargeableUnit());
        shipmentRequest.setVolumetricWeight(autoUpdateWtVolResponse.getVolumetricWeight());
        shipmentRequest.setVolumetricWeightUnit(autoUpdateWtVolResponse.getVolumetricWeightUnit());
        shipmentRequest.setNetWeight(autoUpdateWtVolResponse.getNetWeight());
        shipmentRequest.setNetWeightUnit(autoUpdateWtVolResponse.getNetWeightUnit());
        shipmentRequest.setInnerPacks(autoUpdateWtVolResponse.getInnerPacks());
        shipmentRequest.setInnerPackUnit(autoUpdateWtVolResponse.getInnerPackUnit());

        if(!StringUtility.isEmpty(shipmentRequest.getOrderManagementId())) {
            shipmentRequest.setShipmentOrders(Arrays.asList(ShipmentOrderRequest.builder().orderNumber(shipmentRequest.getOrderManagementNumber()).orderGuid(UUID.fromString(shipmentRequest.getOrderManagementId())).build()));
        }

        if(shipmentRequest.getOrderManagementId()!=null){
            ShipmentDetails shipmentDetails = null;
            shipmentDetails = orderManagementAdapter.getOrderByGuid(shipmentRequest.getOrderManagementId());

            if(shipmentDetails!=null){
                processShipmentRequestFromDetails(shipmentRequest, shipmentDetails);
            }

        }

        return jsonHelper.convertToJson(this.createFromBookingServiceAPI(CommonRequestModel.buildRequest(shipmentRequest)));
    }

    private void processShipmentRequestFromDetails(ShipmentRequest shipmentRequest, ShipmentDetails shipmentDetails) {
        if(shipmentDetails.getGoodsDescription()!=null)
            shipmentRequest.setGoodsDescription(shipmentDetails.getGoodsDescription());

        if(shipmentDetails.getReferenceNumbersList()!=null){
            List<ReferenceNumbersRequest> referenceNumbersList = jsonHelper.convertValue(shipmentDetails.getReferenceNumbersList(), new TypeReference<List<ReferenceNumbersRequest>>() {});
            shipmentRequest.setReferenceNumbersList(referenceNumbersList);
        }

        if(shipmentDetails.getAdditionalDetails()!=null){
            if(shipmentDetails.getAdditionalDetails().getImportBroker()!=null){
                PartiesRequest importBroker = jsonHelper.convertValue(shipmentDetails.getAdditionalDetails().getImportBroker(), PartiesRequest.class);
                shipmentRequest.getAdditionalDetails().setImportBroker(importBroker);
            }

            if(shipmentDetails.getAdditionalDetails().getExportBroker()!=null){
                PartiesRequest exportBroker = jsonHelper.convertValue(shipmentDetails.getAdditionalDetails().getExportBroker(), PartiesRequest.class);
                shipmentRequest.getAdditionalDetails().setExportBroker(exportBroker);
            }
        }
    }

    public boolean isConsoleCreationNeeded(CustomerBookingRequest customerBookingRequest) {
        return (Objects.equals(customerBookingRequest.getTransportType(), Constants.TRANSPORT_MODE_SEA) && Objects.equals(customerBookingRequest.getCargoType(), Constants.CARGO_TYPE_FCL)) ||
                (Objects.equals(customerBookingRequest.getTransportType(), Constants.TRANSPORT_MODE_ROA) &&
                        (Objects.equals(customerBookingRequest.getCargoType(), Constants.CARGO_TYPE_FTL) || Objects.equals(customerBookingRequest.getCargoType(), Constants.CARGO_TYPE_FCL)) ) ||
                (Objects.equals(customerBookingRequest.getTransportType(), Constants.TRANSPORT_MODE_RAI) && Objects.equals(customerBookingRequest.getCargoType(), Constants.CARGO_TYPE_FCL));
    }

    private List<NotesRequest> createNotes(List<Notes> notes){
        if(notes == null) return null;
        return notes.stream().filter(Objects::nonNull).map(note ->
               NotesRequest.builder()
                        .assignedTo(note.getAssignedTo())
                        .label(note.getLabel())
                        .text(note.getText())
                        .insertUserDisplayName(note.getCreatedBy())
                        .isPublic(note.getIsPublic())
                        .insertDate(note.getCreatedAt())
                        .entityType(Constants.CUSTOMER_BOOKING)
                        .build()).toList();
    }

    private List<ReferenceNumbersRequest> createReferenceNumbersList(List<ReferenceNumbersRequest> referenceNumbers){
        if(referenceNumbers == null) return null;
        return referenceNumbers.stream().filter(Objects::nonNull).map(refNumber ->
                ReferenceNumbersRequest.builder()
                        .consolidationId(refNumber.getConsolidationId())
                        .countryOfIssue(refNumber.getCountryOfIssue())
                        .type(refNumber.getType())
                        .referenceNumber(refNumber.getReferenceNumber())
                        .shipmentId(refNumber.getShipmentId())
                        .isPortalEnable(refNumber.getIsPortalEnable())
                        .build()).toList();
    }

    private PartiesRequest createPartiesRequest(PartiesRequest party, String countryCode)
    {
        if(party == null)
            return null;
        return PartiesRequest.builder()
                .addressCode(party.getAddressCode())
                .addressData(party.getAddressData())
                .orgCode(party.getOrgCode())
                .orgData(party.getOrgData())
                .orgId(party.getOrgId())
                .addressId(party.getAddressId())
                .countryCode(countryCode)
                .build();
    }

    private List<PackingRequest> setPackingDetails(List<PackingRequest> packingRequests, String transportMode, Long consolidationId) {
        if(packingRequests != null && !packingRequests.isEmpty()) {
            for (PackingRequest packingRequest : packingRequests) {
                if(!isStringNullOrEmpty(transportMode) && transportMode.equals(Constants.TRANSPORT_MODE_AIR)) {
                    packingRequest.setConsolidationId(consolidationId);
                }
            }
        }
        return packingRequests;
    }

    private List<EventsRequest> setEventDetails(List<EventsRequest> eventsRequestList, ShipmentDetails shipmentDetails) {
        if(eventsRequestList != null && !eventsRequestList.isEmpty()) {
            for (EventsRequest req : eventsRequestList) {
                    req.setShipmentNumber(shipmentDetails.getShipmentId());
                }
            }
        return eventsRequestList;
    }

    private void calculateAutoContainerWeightAndVolume(Set<ContainerRequest> containersList, List<PackingRequest> packingList) throws RunnerException {
        if(containersList != null && !containersList.isEmpty()) {
            for (ContainerRequest containers : containersList) {
                if(packingList != null) {
                    List<PackingRequest> packings = packingList.stream().filter(packing -> Objects.equals(packing.getContainerId(), containers.getId())).toList();
                    setGrossWeightVolInContainers(containers, packings);
                }
            }
        }
    }

    private void setGrossWeightVolInContainers(ContainerRequest containers, List<PackingRequest> packings) throws RunnerException {
        BigDecimal totalWeight = BigDecimal.ZERO;
        BigDecimal totalVolume = BigDecimal.ZERO;
        if(packings != null && !packings.isEmpty()) {
            setGrossWvInContainers(containers);
            for (PackingRequest packing : packings) {
                totalWeight = getTotalWeightFromWeightUnit(containers, packing, totalWeight);
                totalVolume = getTotalVolFromWeightUnit(containers, packing, totalVolume);
            }
            containers.setGrossWeight(totalWeight);
            containers.setGrossVolume(totalVolume);
        }
    }

    private void setGrossWvInContainers(ContainerRequest containers) {
        if(isStringNullOrEmpty(containers.getGrossWeightUnit()))
            containers.setGrossWeightUnit(Constants.WEIGHT_UNIT_KG);
        if(isStringNullOrEmpty(containers.getGrossVolumeUnit()))
            containers.setGrossVolumeUnit(Constants.VOLUME_UNIT_M3);
    }

    private BigDecimal getTotalVolFromWeightUnit(ContainerRequest containers, PackingRequest packing, BigDecimal totalVolume) throws RunnerException {
        if(!isStringNullOrEmpty(packing.getVolumeUnit()))
            totalVolume = totalVolume.add(new BigDecimal(convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), containers.getGrossVolumeUnit()).toString()));
        return totalVolume;
    }

    private BigDecimal getTotalWeightFromWeightUnit(ContainerRequest containers, PackingRequest packing, BigDecimal totalWeight) throws RunnerException {
        if(!isStringNullOrEmpty(packing.getWeightUnit()))
            totalWeight = totalWeight.add(new BigDecimal(convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), containers.getGrossWeightUnit()).toString()));
        return totalWeight;
    }

    private void callChangeShipmentDGStatusFromPack(ShipmentDetails shipmentDetails, PackingRequest pack, Packing oldPacking) {
        boolean isDGClass1 = commonUtils.checkIfDGClass1(pack.getDGClass());
        if(pack.getId() == null) {
            commonUtils.changeShipmentDGStatusToReqd(shipmentDetails, isDGClass1);
            return;
        }
        if(oldPacking != null && commonUtils.checkIfDGFieldsChangedInPacking(pack, oldPacking))
            commonUtils.changeShipmentDGStatusToReqd(shipmentDetails, isDGClass1);
    }

    private void changeShipmentDGValuesFromPack(PackingRequest pack, Set<Long> dgConts, ShipmentDetails shipmentDetails,
                                                Map<Long, Packing> oldPacksMap, Set<Long> newPackAttachedInConts) {
        Packing oldPacking = null;
        if(oldPacksMap.containsKey(pack.getId()))
            oldPacking = oldPacksMap.get(pack.getId());
        if(Boolean.TRUE.equals(pack.getHazardous())) {
            dgConts.add(pack.getContainerId());
            callChangeShipmentDGStatusFromPack(shipmentDetails, pack, oldPacking);
        }
        if(!Objects.isNull(pack.getContainerId()) &&
                ( (!Objects.isNull(oldPacking) && !Objects.equals(pack.getContainerId(), oldPacking.getContainerId())) || Objects.isNull(oldPacking) ))
            newPackAttachedInConts.add(pack.getContainerId());
    }

    private void changeDGStatusFromPacks(List<PackingRequest> packingList, Set<Long> dgConts, ShipmentDetails shipmentDetails,
                                         ShipmentDetails oldEntity, Set<Long> newPackAttachedInConts) {
        Map<Long, Packing> oldPacksMap = new HashMap<>();
        if(!Objects.isNull(oldEntity))
            oldPacksMap = oldEntity.getPackingList().stream().collect(Collectors.toMap(e -> e.getId(), c -> c));
        if(Objects.isNull(packingList))
            return;
        for(PackingRequest pack: packingList) {
            changeShipmentDGValuesFromPack(pack, dgConts, shipmentDetails, oldPacksMap, newPackAttachedInConts);
        }
    }

    private void callChangeShipmentDGStatusFromContainer(ShipmentDetails shipmentDetails, ContainerRequest container,
                                                         Map<Long, Containers> oldContainersMap, Set<Long> newPackAttachedInConts) {
        Containers oldContainer = null;
        boolean isDGClass1 = commonUtils.checkIfDGClass1(container.getDgClass());
        if(container.getId() == null) {
            commonUtils.changeShipmentDGStatusToReqd(shipmentDetails, isDGClass1);
        }
        if(container.getId() != null) {
            if(oldContainersMap.containsKey(container.getId()))
                oldContainer = oldContainersMap.get(container.getId());
            if(oldContainer != null && commonUtils.checkIfDGFieldsChangedInContainer(container, oldContainer)) {
                commonUtils.changeShipmentDGStatusToReqd(shipmentDetails, isDGClass1);
            }
            if(newPackAttachedInConts.contains(container.getId()))
                commonUtils.changeShipmentDGStatusToReqd(shipmentDetails, isDGClass1);
        }
    }

    private void changeShipmentDGValuesFromContainer(Set<Long> dgConts, ShipmentDetails shipmentDetails,
                                                     Set<Long> newPackAttachedInConts, ContainerRequest container, Map<Long, Containers> oldContainersMap) {
        if(!Objects.isNull(container.getId()) && dgConts.contains(container.getId())) {
            container.setHazardous(true);
            if(isStringNullOrEmpty(container.getDgClass()) || isStringNullOrEmpty(container.getUnNumber()) || isStringNullOrEmpty(container.getProperShippingName()))
                throw new ValidationException(OCEAN_DG_CONTAINER_FIELDS_VALIDATION);
        }
        if(Boolean.TRUE.equals(container.getHazardous())) {
            callChangeShipmentDGStatusFromContainer(shipmentDetails, container, oldContainersMap, newPackAttachedInConts);
        }
    }

    private void changeDGStatusFromContainers(Set<ContainerRequest> containersList, Set<Long> dgConts,
                                              ShipmentDetails shipmentDetails, ShipmentDetails oldEntity,
                                              Set<Long> newPackAttachedInConts) {
        Map<Long, Containers> oldContainersMap = new HashMap<>();
        if(!Objects.isNull(oldEntity))
            oldContainersMap = oldEntity.getContainersList().stream().collect(Collectors.toMap(e -> e.getId(), c -> c));
        if(Objects.isNull(containersList))
            return;
        for(ContainerRequest container: containersList) {
            changeShipmentDGValuesFromContainer(dgConts, shipmentDetails, newPackAttachedInConts, container, oldContainersMap);
        }
    }

    private void makeDGOceanChangesFromPacksAndContainers(Set<ContainerRequest> containersList, List<PackingRequest> packingList, ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        Set<Long> dgConts = new HashSet<>();
        Set<Long> newPackAttachedInConts = new HashSet<>();
        changeDGStatusFromPacks(packingList, dgConts, shipmentDetails, oldEntity, newPackAttachedInConts);
        dgConts.remove(null);
        changeDGStatusFromContainers(containersList, dgConts, shipmentDetails, oldEntity, newPackAttachedInConts);
    }

    public ResponseEntity<IRunnerResponse> calculateAutoUpdateWtVolInShipment(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        try {
            AutoUpdateWtVolRequest request = (AutoUpdateWtVolRequest) commonRequestModel.getData();
            AutoUpdateWtVolResponse response = calculateShipmentWV(request);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> calculateWtVolInShipmentOnChanges(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        try {
            AutoUpdateWtVolRequest request = (AutoUpdateWtVolRequest) commonRequestModel.getData();
            AutoUpdateWtVolResponse response = jsonHelper.convertValue(request, AutoUpdateWtVolResponse.class);
            response = calculateVW(request, response, true);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private AutoUpdateWtVolResponse calculateShipmentWV(AutoUpdateWtVolRequest request) throws RunnerException {
        AutoUpdateWtVolResponse response = jsonHelper.convertValue(request, AutoUpdateWtVolResponse.class);
        List<Packing> packingList = new ArrayList<>();
        if(request.getPackingList() != null)
            packingList = jsonHelper.convertValueToList(request.getPackingList(), Packing.class);
        List<Containers> containersList = new ArrayList<>();
        if(request.getContainersList() != null)
            containersList = jsonHelper.convertValueToList(request.getContainersList(), Containers.class);
        calculatePacksAndPacksUnit(packingList, response);
        response = calculateWeightAndVolumeUnit(request, packingList, response);
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        boolean isPacksPresent = packingList != null && !packingList.isEmpty();
        if(!isPacksPresent)
            response = updateShipmentDetails(response, containersList);
        calculateVW(request, response, true);
        if(shipmentSettingsDetails.getIsShipmentLevelContainer() == null || !shipmentSettingsDetails.getIsShipmentLevelContainer().booleanValue()
                || Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR) || isPacksPresent) {
            ShipmentMeasurementDetailsDto dto = new ShipmentMeasurementDetailsDto();
            response.setPackSummary(packingService.calculatePackSummary(packingList, request.getTransportMode(), request.getShipmentType(), dto));
            updateResponseFromDto(request, response, dto, shipmentSettingsDetails);
        }
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        if(Boolean.TRUE.equals(v1TenantSettingsResponse.getP100Branch()) && Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_SEA)) {
            calculatePacksAndPacksUnitFromContainer(response, containersList);
        }
        return response;
    }

    private void updateResponseFromDto(AutoUpdateWtVolRequest request, AutoUpdateWtVolResponse response, ShipmentMeasurementDetailsDto dto, ShipmentSettingsDetails shipmentSettingsDetails) {
        if((Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_SEA) &&
                Objects.equals(request.getShipmentType(), Constants.SHIPMENT_TYPE_LCL)) ||
                Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) {
            response.setInnerPacks(dto.getInnerPacks());
            response.setInnerPackUnit(dto.getInnerPackUnit());
        }
        if(shipmentSettingsDetails.getIsShipmentLevelContainer() != null && shipmentSettingsDetails.getIsShipmentLevelContainer()
        && request.getPackingList() != null && !request.getPackingList().isEmpty()) {
            response.setWeight(dto.getWeight());
            response.setWeightUnit(dto.getWeightUnit());
            response.setVolume(dto.getVolume());
            response.setVolumeUnit(dto.getVolumeUnit());
            response.setNetWeight(dto.getNetWeight());
            response.setNetWeightUnit(dto.getNetWeightUnit());
            response.setNoOfPacks(dto.getNoOfPacks());
            response.setPacksUnit(dto.getPacksUnit());
        }
        else if(shipmentSettingsDetails.getIsShipmentLevelContainer() == null || !shipmentSettingsDetails.getIsShipmentLevelContainer()){
            response.setNoOfPacks(dto.getNoOfPacks());
            response.setPacksUnit(dto.getPacksUnit());
        }
    }

    private AutoUpdateWtVolResponse updateShipmentDetails(AutoUpdateWtVolResponse response, List<Containers> containersList) throws RunnerException { // to account for updateShipmentDetails flag in v1 container summary
        double totalWeight = 0;
        double packageCount = 0;
        double tareWeight = 0;
        double totalVolume = 0;
        double totalContainerCount = 0;
        int totalPacks = 0;
        String packsUnit = "";
        String toWeightUnit = Constants.WEIGHT_UNIT_KG;
        String toVolumeUnit = Constants.VOLUME_UNIT_M3;
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        toWeightUnit = getToWeightUnit(shipmentSettingsDetails, toWeightUnit);
        toVolumeUnit = getToVolumeUnit(shipmentSettingsDetails, toVolumeUnit);
        if(containersList != null) {
            for (Containers containers : containersList) {
                double wInDef = convertUnit(Constants.MASS, containers.getGrossWeight(), containers.getGrossWeightUnit(), toWeightUnit).doubleValue();
                double tarDef = convertUnit(Constants.MASS, containers.getTareWeight(), containers.getTareWeightUnit(), toWeightUnit).doubleValue();
                double volume = convertUnit(Constants.VOLUME, containers.getGrossVolume(), containers.getGrossVolumeUnit(), toVolumeUnit).doubleValue();
                totalWeight = totalWeight + wInDef;
                tareWeight = tareWeight + tarDef;
                if(!isStringNullOrEmpty(containers.getPacks()))
                    packageCount = packageCount + Long.parseLong(containers.getPacks());
                totalVolume = totalVolume + volume;
                if(containers.getContainerCount() != null)
                    totalContainerCount = totalContainerCount + containers.getContainerCount();
                if(!isStringNullOrEmpty(containers.getPacks()))
                    totalPacks = totalPacks + Integer.parseInt(containers.getPacks());
            }
        }
        if (!containersList.isEmpty() ) {
            packsUnit = setPacksUnit(containersList);
        }
        response.setWeight(BigDecimal.valueOf(totalWeight));
        response.setVolume(BigDecimal.valueOf(totalVolume));
        response.setWeightUnit(toWeightUnit);
        response.setVolumeUnit(toVolumeUnit);
        response.setNoOfPacks(totalPacks == 0 ? null : String.valueOf(totalPacks));
        response.setPacksUnit(packsUnit);
        response.setNetWeight(BigDecimal.valueOf(tareWeight));
        response.setNetWeightUnit(toWeightUnit);
        return response;
    }

    private void calculatePacksAndPacksUnitFromContainer(AutoUpdateWtVolResponse response, List<Containers> containersList) {
        if(containersList != null && !containersList.isEmpty()) {
            String packsUnit = "";
            long packageCount = 0;
            long totalPacks = 0;
            for (Containers container : containersList) {
                if (!isStringNullOrEmpty(container.getPacks())) {
                    packageCount = packageCount + Integer.parseInt(container.getPacks());
                    totalPacks = totalPacks + Integer.parseInt(container.getPacks());
                }
            }
            packsUnit = setPacksUnit(containersList);
            response.setNoOfPacks(totalPacks == 0 ? null : String.valueOf(totalPacks));
            response.setPacksUnit(packsUnit);
        }
    }

    private String setPacksUnit(List<Containers> containersList) {
        String firstPacksType = containersList.get(0).getPacksType();
        boolean isSame = containersList.stream()
                .map(Containers::getPacksType)
                .allMatch(packsType -> packsType == null || packsType.equals(firstPacksType));

        if (isSame) {
            return firstPacksType;
        } else {
            return Constants.MPK;
        }
    }

    private AutoUpdateWtVolResponse calculateWeightAndVolumeUnit(AutoUpdateWtVolRequest request, List<Packing> packings, AutoUpdateWtVolResponse response) throws RunnerException {
        BigDecimal totalWeight = BigDecimal.ZERO;
        BigDecimal totalVolume = BigDecimal.ZERO;
        if(isStringNullOrEmpty(request.getWeightUnit()))
            response.setWeightUnit(Constants.WEIGHT_UNIT_KG);
        if(isStringNullOrEmpty(request.getVolumeUnit()))
            response.setVolumeUnit(Constants.VOLUME_UNIT_M3);
        if(packings != null && !packings.isEmpty()) {
            for (Packing packing : packings) {
                if(packing.getWeight() != null && !isStringNullOrEmpty(packing.getWeightUnit())) {
                    totalWeight = totalWeight.add(new BigDecimal(convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), response.getWeightUnit()).toString()));
                }
                if(packing.getVolume() != null && !isStringNullOrEmpty(packing.getVolumeUnit())) {
                    totalVolume = totalVolume.add(new BigDecimal(convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), response.getVolumeUnit()).toString()));
                }
            }
            response.setWeight(totalWeight);
            response.setVolume(totalVolume);
            response = calculateVW(request, response, false);
        }
        return response;
    }

    private AutoUpdateWtVolResponse calculateVW(AutoUpdateWtVolRequest request, AutoUpdateWtVolResponse response, boolean recalculateVwObInKgAndM3) throws RunnerException{
        if(isStringNullOrEmpty(request.getTransportMode()))
            return response;
        if(!isStringNullOrEmpty(response.getWeightUnit()) && !isStringNullOrEmpty(response.getVolumeUnit())) {
            VolumeWeightChargeable vwOb = consolidationService.calculateVolumeWeight(request.getTransportMode(), response.getWeightUnit(), response.getVolumeUnit(), response.getWeight(), response.getVolume());
            response.setChargable(vwOb.getChargeable());
            if(request.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                response.setChargable(BigDecimal.valueOf(roundOffAirShipment(response.getChargable().doubleValue())));
            }
            response.setChargeableUnit(vwOb.getChargeableUnit());
            if(request.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) && !isStringNullOrEmpty(request.getShipmentType()) && request.getShipmentType().equals(Constants.SHIPMENT_TYPE_LCL)) {
                double volInM3 = convertUnit(Constants.VOLUME, response.getVolume(), response.getVolumeUnit(), Constants.VOLUME_UNIT_M3).doubleValue();
                double wtInKg = convertUnit(Constants.MASS, response.getWeight(), response.getWeightUnit(), Constants.WEIGHT_UNIT_KG).doubleValue();
                response.setChargable(BigDecimal.valueOf(Math.max(wtInKg/1000, volInM3)));
                response.setChargeableUnit(Constants.VOLUME_UNIT_M3);
                if(recalculateVwObInKgAndM3)
                    vwOb = consolidationService.calculateVolumeWeight(request.getTransportMode(), Constants.WEIGHT_UNIT_KG, Constants.VOLUME_UNIT_M3, BigDecimal.valueOf(wtInKg), BigDecimal.valueOf(volInM3));
            }

            response.setVolumetricWeight(vwOb.getVolumeWeight());
            response.setVolumetricWeightUnit(vwOb.getVolumeWeightUnit());
        }
        return response;
    }

    private double roundOffAirShipment(double charge) {
        if (charge - 0.50 <= Math.floor(charge) && charge != Math.floor(charge)) {
            charge = Math.floor(charge) + 0.5;
        } else {
            charge = Math.ceil(charge);
        }
        return charge;
    }

    private <T> T calculatePacksAndPacksUnit(List<Packing> packings, T response) {
        Integer totalPacks = 0;
        String tempPackingUnit = null;
        String packingUnit = null;
        if(packings != null && !packings.isEmpty()) {
            for (Packing packing : packings) {
                if(!isStringNullOrEmpty(packing.getPacks()))
                    totalPacks = totalPacks + Integer.parseInt(packing.getPacks());
                if (tempPackingUnit == null) {
                    tempPackingUnit = packing.getPacksType();
                    packingUnit = packing.getPacksType();
                }
                else {
                    if(isMPKUnitCase(packing, tempPackingUnit)) {
                        packingUnit = Constants.MPK;
                    }
                }
            }
        }
        getResponseForPacks(response, totalPacks, packingUnit);
        return response;
    }

    private boolean isMPKUnitCase(Packing packing, String tempPackingUnit) {
        return !isStringNullOrEmpty(packing.getPacksType()) && tempPackingUnit.equals(packing.getPacksType());
    }

    private <T> void getResponseForPacks(T response, Integer totalPacks, String packingUnit) {
        if(response instanceof AutoUpdateWtVolResponse autoUpdateWtVolResponse) {
            autoUpdateWtVolResponse.setNoOfPacks(totalPacks.toString());
            autoUpdateWtVolResponse.setPacksUnit(packingUnit);
        } else if(response instanceof MeasurementBasisResponse measurementBasisResponseas) {
            measurementBasisResponseas.setPackCount(totalPacks);
        }
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException {
        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for Shipment update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException("Request can't be null");
        }

        Optional<ShipmentDetails>oldEntity =retrieveByIdOrGuid(request);
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        ShipmentDetails entity = objectMapper.convertValue(request, ShipmentDetails.class);
        entity.setId(oldEntity.get().getId());
        if(entity.getGuid() != null && !oldEntity.get().getGuid().equals(entity.getGuid())) {
            throw new RunnerException("Provided GUID doesn't match with the existing one !");
        }
        if (entity.getContainersList() == null)
            entity.setContainersList(oldEntity.get().getContainersList());
        // update Ata/Atd in shipment from events
        eventService.updateAtaAtdInShipment(entity.getEventsList(), entity, shipmentSettingsDetails);
        entity = shipmentDao.update(entity, false);
        dependentServiceHelper.pushShipmentDataToDependentService(entity, false, Boolean.TRUE.equals(request.getIsAutoSellRequired()), oldEntity.get().getContainersList());
        try {
            shipmentSync.sync(entity, null, null, entity.getGuid().toString(), false);
        } catch (Exception e){
            log.error(SyncingConstants.ERROR_SYNCING_SHIPMENTS, e);
        }
        return ResponseHelper.buildSuccessResponse(objectMapper.convertValue(entity, ShipmentDetailsResponse.class));
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> completeUpdate(CommonRequestModel commonRequestModel) throws RunnerException {

        ShipmentRequest shipmentRequest = (ShipmentRequest) commonRequestModel.getData();
        this.setColoadingStation(shipmentRequest);
        ShipmentDetailsResponse response = completeUpdateShipment(shipmentRequest, false);
        return ResponseHelper.buildSuccessResponse(response);
    }

    private ShipmentDetailsResponse completeUpdateShipment(ShipmentRequest shipmentRequest, boolean isFromET) throws RunnerException {
        long start = System.currentTimeMillis();
        log.info("{} | starts completeUpdateShipment....", LoggerHelper.getRequestIdFromMDC());
        long mid = System.currentTimeMillis();
        Optional<ShipmentDetails> oldEntity = retrieveByIdOrGuid(shipmentRequest);
        log.info("{} | completeUpdateShipment db query: retrieveByIdOrGuid.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);
        if (oldEntity.isEmpty()) {
            log.debug(ShipmentConstants.SHIPMENT_DETAILS_NULL_FOR_ID_ERROR, shipmentRequest.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        try {
            mid = System.currentTimeMillis();
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            ShipmentDetails entity = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
            log.info("{} | completeUpdateShipment object mapper request.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);
            entity.setId(oldEntity.get().getId());
            List<Long> removedConsolIds = new ArrayList<>();
            MutableBoolean isNewConsolAttached = new MutableBoolean(false);

            mid = System.currentTimeMillis();
            ShipmentDetails oldConvertedShipment = jsonHelper.convertValue(oldEntity.get(), ShipmentDetails.class);
            log.info("{} | completeUpdateShipment object mapper old entity.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);

            if(Objects.equals(Constants.SHIPMENT_TYPE_DRT, entity.getJobType()) && !Objects.equals(oldEntity.get().getJobType(), entity.getJobType()) &&  checkIfAlreadyPushRequested(oldEntity.get())) {
                throw new ValidationException(ErrorConstants.VALIDATE_JOB_TYPE_CHANGE);
            }
            mid = System.currentTimeMillis();
            boolean syncConsole = beforeSave(entity, oldEntity.get(), false, shipmentRequest, shipmentSettingsDetails, removedConsolIds, isNewConsolAttached, false);
            log.info("{} | completeUpdateShipment before save.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);
            mid = System.currentTimeMillis();
            entity = shipmentDao.update(entity, false);
            log.info("{} | completeUpdateShipment Update.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);
            mid = System.currentTimeMillis();
            createAuditLog(entity, jsonHelper.convertToJson(oldConvertedShipment), DBOperationType.UPDATE.name());
            log.info("{} | completeUpdateShipment auditLog.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);

            mid = System.currentTimeMillis();
            afterSave(entity, oldConvertedShipment, false, shipmentRequest, shipmentSettingsDetails, syncConsole, removedConsolIds, isNewConsolAttached, false, isFromET);
            log.info("{} | completeUpdateShipment after save.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);
            ShipmentDetails finalEntity1 = entity;
            String entityPayload = jsonHelper.convertToJson(finalEntity1);
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.createLogHistoryForShipment(entityPayload, finalEntity1.getId(), finalEntity1.getGuid())), executorServiceMasterData);
            log.info("end completeUpdateShipment.... {} ms", System.currentTimeMillis() - start);
            return shipmentDetailsMapper.map(entity);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error("Error occurred due to: " + e.getStackTrace());
            log.error(responseMsg, e);
            throw new ValidationException(e.getMessage());
        }
    }

    @Override
    public ShipmentDetailsResponse completeUpdateShipmentFromEntityTransfer(ShipmentRequest shipmentRequest) throws RunnerException {
        return this.completeUpdateShipment(shipmentRequest, true);
    }

    private void setColoadingStation(ShipmentRequest request) {
        var tenantSettings = commonUtils.getCurrentTenantSettings();
        if(Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR)
                && Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
            commonUtils.setInterBranchContextForColoadStation();
        }
    }

    private void setColoadingStation(ShipmentDetails request) {
        var tenantSettings = commonUtils.getCurrentTenantSettings();
        if(Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && Objects.equals(request.getDirection(), Constants.DIRECTION_EXP)
                && Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
            commonUtils.setInterBranchContextForColoadStation();
        }
    }

    public void createLogHistoryForShipment(String entityPayload, Long id, UUID guid){
        try {
            logsHistoryService.createLogHistory(LogHistoryRequest.builder().entityId(id)
                    .entityType(Constants.SHIPMENT).entityGuid(guid).entityPayload(entityPayload).build());
        } catch (Exception ex) {
            log.error("Error while creating LogsHistory for Shipment: " + ex.getMessage());
        }
    }

    private void syncShipment(ShipmentDetails shipmentDetails, Hbl hbl, List<UUID> deletedContGuids, List<Packing> packsForSync, ConsolidationDetails consolidationDetails, boolean syncConsole) {
        String transactionId = shipmentDetails.getGuid().toString();
        try {
            shipmentSync.sync(shipmentDetails, deletedContGuids, null, transactionId, false);
        } catch (Exception e){
            log.error(SyncingConstants.ERROR_SYNCING_SHIPMENTS, e);
        }
        if(hbl != null) {
            try {
                hblSync.sync(hbl, transactionId);
            }
            catch (Exception e) {
                log.error("Error performing sync on hbl entity, {}", e);
            }
        }
        if(syncConsole && consolidationDetails != null) {
            try {
                consolidationSync.sync(consolidationDetails, transactionId, false);
            } catch (Exception e) {
                log.error("Error performing sync on consol entity, {}", e);
            }
        }
        if(packsForSync != null) {
            try {
                packingsSync.sync(packsForSync, transactionId);
            } catch (Exception e) {
                log.error("Error performing sync on packings list, {}", e);
            }
        }
    }

    @SuppressWarnings("java:S125")
    private boolean beforeSave(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, ShipmentRequest shipmentRequest, ShipmentSettingsDetails shipmentSettingsDetails, List<Long> removedConsolIds, MutableBoolean isNewConsolAttached, boolean isImportFile) throws RunnerException{
        validateStaleShipmentUpdateError(shipmentDetails, isCreate);

        CompletableFuture<Void> populateUnlocCodeFuture = getPopulateUnlocCodeFuture(shipmentDetails, oldEntity);

        List<Long> tempConsolIds = new ArrayList<>();
        Long id = !Objects.isNull(oldEntity) ? oldEntity.getId() : null;
        boolean syncConsole = false;

        processVoyageAndFlightNumber(shipmentDetails);

        if (Objects.isNull(shipmentDetails.getSourceTenantId()))
            shipmentDetails.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));

        Set<ConsolidationDetailsRequest> consolidationDetailsRequests = shipmentRequest.getConsolidationList();
        List<Routings> mainCarriageRoutings = (shipmentDetails.getRoutingsList() != null ? shipmentDetails.getRoutingsList().stream().filter(i -> RoutingCarriage.MAIN_CARRIAGE.equals(i.getCarriage())).toList() : Collections.emptyList());
        boolean isRouteMasterEnabled = Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableRouteMaster());
        if (Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getIsRunnerV3Enabled()) && Boolean.TRUE.equals(isRouteMasterEnabled) && mainCarriageRoutings != null && !mainCarriageRoutings.isEmpty() && shouldSetPorts(shipmentRequest)) {
            shipmentDetails.getCarrierDetails().setOriginPort(mainCarriageRoutings.get(0).getPol());
            shipmentDetails.getCarrierDetails().setDestinationPort(mainCarriageRoutings.get(mainCarriageRoutings.size() - 1).getPod());
        }
        tempConsolIds = processConsolidationDetailsRequests(shipmentDetails, oldEntity, removedConsolIds, isNewConsolAttached, consolidationDetailsRequests, tempConsolIds);

        processDGValidations(shipmentDetails, oldEntity, removedConsolIds, isNewConsolAttached, consolidationDetailsRequests);

        List<PackingRequest> packingRequest = shipmentRequest.getPackingList();
        Set<ContainerRequest> containerRequest = shipmentRequest.getContainersList();

        containerRequest = getContainerRequestsForRemovedConsolIds(shipmentDetails, oldEntity, shipmentRequest, removedConsolIds, containerRequest);

        containerRequest = getContainerRequestsForAutoWeightVolumeUpdate(shipmentDetails, oldEntity, packingRequest, containerRequest);
        if (Constants.TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode()))
            makeDGOceanChangesFromPacksAndContainers(containerRequest, packingRequest, shipmentDetails, oldEntity);

        Long consolidationId = getConsolidationIdFromShipment(shipmentDetails);
        Set<Containers> updatedContainers = getUpdatedContainers(shipmentDetails, oldEntity, isCreate, containerRequest, consolidationId, id);
        shipmentDetails.setContainersList(updatedContainers);

        syncConsole = getUpdatedSyncConsole(shipmentDetails, isCreate, shipmentRequest, shipmentSettingsDetails, updatedContainers, tempConsolIds, syncConsole);

        if(Boolean.TRUE.equals(shipmentRequest.getIsChargableEditable())){
            shipmentDetails.setChargable(shipmentRequest.getChargable());
        }
        validateBeforeSave(shipmentDetails);


        processIsNewConsolAttached(shipmentDetails, isCreate, isNewConsolAttached, isRouteMasterEnabled, mainCarriageRoutings);

        processBranchesAndPartner(shipmentDetails);

        if(Objects.equals(shipmentDetails.getJobType(), Constants.SHIPMENT_TYPE_DRT) && Boolean.TRUE.equals(shipmentDetails.getAdditionalDetails().getDraftPrinted())
                && Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_SEA) && Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP)) {
            List<Hbl> hbls = hblDao.findByShipmentId(shipmentDetails.getId());
            if(!hbls.isEmpty()) {
                hblDao.delete(hbls.get(0));
            }
            shipmentDetails.getAdditionalDetails().setDraftPrinted(false);
        }
        if(checkOriginalPrintedForJobTypeChange(shipmentDetails, oldEntity)){
            throw new ValidationException("Consolidation type cannot be changed as the original BL has been generated for this shipment.");
        }
        updateAwbForDisableFetchConditionForAwb(shipmentDetails, oldEntity, shipmentSettingsDetails);

        if(checkIfLCLConsolidationEligible(shipmentDetails))
            updateShipmentGateInDateAndStatusFromPacks(packingRequest, shipmentDetails);

        var tenantSettings = Optional.ofNullable(commonUtils.getCurrentTenantSettings()).orElse(V1TenantSettingsResponse.builder().build());
        // If TransportModeConfig flag is ON, this block will check for the valid transport mode
        validTransportModeForTrasnportModeConfig(shipmentDetails, oldEntity, isCreate, isImportFile, tenantSettings);


        // Ignore events payload to avoid transaction issues bypassing shipmentDetailsDao.update(...);
        // Update happens in after save from request body
        shipmentDetails.setEventsList(null);

        populateUnlocCodeFuture.join();
        return syncConsole;
    }

    private void processVoyageAndFlightNumber(ShipmentDetails shipmentDetails) {
        if (shipmentDetails.getCarrierDetails() != null) {
            if (shipmentDetails.getTransportMode() != null && shipmentDetails.getTransportMode().equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR)) {
                shipmentDetails.getCarrierDetails().setVoyage(null);
            } else {
                shipmentDetails.getCarrierDetails().setFlightNumber(null);
            }
        }
    }

    private void processDGValidations(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Long> removedConsolIds, MutableBoolean isNewConsolAttached, Set<ConsolidationDetailsRequest> consolidationDetailsRequests) throws RunnerException {
        if (Constants.TRANSPORT_MODE_AIR.equals(shipmentDetails.getTransportMode()))
            airDGValidations(shipmentDetails, oldEntity, removedConsolIds, isNewConsolAttached, consolidationDetailsRequests);
        if (Boolean.TRUE.equals(shipmentDetails.getContainsHazardous()) && (Objects.isNull(oldEntity) || !Boolean.TRUE.equals(oldEntity.getContainsHazardous())) &&
                !Boolean.TRUE.equals(isNewConsolAttached.getValue()) && !setIsNullOrEmpty(shipmentDetails.getConsolidationList())) {
            ConsolidationDetails consolidationDetails1 = shipmentDetails.getConsolidationList().iterator().next();
            dgValidations(shipmentDetails, consolidationDetails1, 0);
        }
    }

    private Set<ContainerRequest> getContainerRequestsForAutoWeightVolumeUpdate(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<PackingRequest> packingRequest, Set<ContainerRequest> containerRequest) throws RunnerException {
        if (shipmentDetails.getContainerAutoWeightVolumeUpdate() != null && shipmentDetails.getContainerAutoWeightVolumeUpdate().booleanValue() && packingRequest != null) {
            if (Objects.isNull(containerRequest) && !Objects.isNull(oldEntity))
                containerRequest = new HashSet<>(jsonHelper.convertValueToList(oldEntity.getContainersList().stream().toList(), ContainerRequest.class));
            calculateAutoContainerWeightAndVolume(containerRequest, packingRequest);
        }
        return containerRequest;
    }

    private void validTransportModeForTrasnportModeConfig(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, boolean isImportFile, V1TenantSettingsResponse tenantSettings) {
        if (Boolean.TRUE.equals(tenantSettings.getTransportModeConfig()) && Boolean.FALSE.equals(isImportFile) && (isCreate || !Objects.equals(oldEntity.getTransportMode(), shipmentDetails.getTransportMode()))
                    && Boolean.FALSE.equals(commonUtils.isTransportModeValid(shipmentDetails.getTransportMode(), Constants.SHIPMENT_DETAILS, tenantSettings))) {
                    throw new ValidationException(String.format(ErrorConstants.INVALID_TRANSPORT_MODE, shipmentDetails.getTransportMode()));
        }
    }

    private void updateAwbForDisableFetchConditionForAwb(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, ShipmentSettingsDetails shipmentSettingsDetails) throws RunnerException {
        if(checkDisableFetchConditionForAwb(shipmentDetails, oldEntity, shipmentSettingsDetails)) {
            List<Awb> awbs = awbDao.findByShipmentId(shipmentDetails.getId());
            if(!awbs.isEmpty()) {
                Awb awb = awbs.get(0);
                awb.getAwbGoodsDescriptionInfo().forEach(x -> {
                    x.setDisableFetchRates(false);
                    x.setEnableFetchRatesWarning(true);
                });
                awbDao.save(awb);
            }
        }
    }

    private Set<ContainerRequest> getContainerRequestsForRemovedConsolIds(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, ShipmentRequest shipmentRequest, List<Long> removedConsolIds, Set<ContainerRequest> containerRequest) throws RunnerException {
        if (removedConsolIds != null && !removedConsolIds.isEmpty()) {
            shipmentDetails.setConsolRef(null);
            List<Containers> allConsolConts = new ArrayList<>();
            List<Containers> containersList = containerDao.findByConsolidationIdIn(removedConsolIds);
            if (containersList != null && !containersList.isEmpty()) {
                allConsolConts.addAll(containersList);
                if (Objects.isNull(containerRequest) && !Objects.isNull(oldEntity)) {
                    containerRequest = new HashSet<>(jsonHelper.convertValueToList(oldEntity.getContainersList().stream().toList(),ContainerRequest.class));
                }
                if (Objects.nonNull(containerRequest)) {
                    containerRequest.removeIf(obj2 -> allConsolConts.stream().anyMatch(obj1 -> obj1.getId().equals(obj2.getId())));
                }
                changeContainerWtVolOnDetach(shipmentRequest, allConsolConts);
            }

        }
        return containerRequest;
    }

    private void processBranchesAndPartner(ShipmentDetails shipmentDetails) {
        if (shipmentDetails.getReceivingBranch() != null && shipmentDetails.getReceivingBranch() == 0) {
            shipmentDetails.setReceivingBranch(null);
        }
        if(shipmentDetails.getReceivingBranch() == null && !setIsNullOrEmpty(shipmentDetails.getConsolidationList()) && Boolean.TRUE.equals(shipmentDetails.getConsolidationList().iterator().next().getInterBranchConsole())){
            shipmentDetails.setReceivingBranch(shipmentDetails.getConsolidationList().iterator().next().getReceivingBranch());
            shipmentDetails.setIsReceivingBranchAdded(false);
        }
        if (ObjectUtils.isNotEmpty(shipmentDetails.getTriangulationPartnerList())
                && shipmentDetails.getTriangulationPartnerList().size() == 1) {
            TriangulationPartner triangulationPartner = shipmentDetails.getTriangulationPartnerList().get(0);
            if (triangulationPartner != null
                    && Long.valueOf(0).equals(triangulationPartner.getTriangulationPartner())) {
                shipmentDetails.setTriangulationPartnerList(null);
            }
        } else if (shipmentDetails.getTriangulationPartnerList() == null
                && shipmentDetails.getTriangulationPartner() != null
                && shipmentDetails.getTriangulationPartner() == 0) {
            shipmentDetails.setTriangulationPartner(null);
        }
        if(shipmentDetails.getDocumentationPartner() != null && shipmentDetails.getDocumentationPartner() == 0)
            shipmentDetails.setDocumentationPartner(null);
    }

    private void processIsNewConsolAttached(ShipmentDetails shipmentDetails, boolean isCreate, MutableBoolean isNewConsolAttached, boolean isRouteMasterEnabled, List<Routings> mainCarriageRoutings) throws RunnerException {
        if (Boolean.TRUE.equals(isNewConsolAttached.getValue())) {
            ConsolidationDetails consolidationDetails1 = shipmentDetails.getConsolidationList().iterator().next();
            List<Routings> routings = routingsDao.findRoutingsByConsolidationId(consolidationDetails1.getId());
            consolidationService.syncMainCarriageRoutingToShipment(routings, shipmentDetails, false, false);
            dgValidations(shipmentDetails, consolidationDetails1, 1);
            if (shipmentDetails.getCargoDeliveryDate() != null && consolidationDetails1.getLatDate() != null && consolidationDetails1.getLatDate().isAfter(shipmentDetails.getCargoDeliveryDate())) {
                throw new RunnerException("Cargo Delivery Date is lesser than LAT Date.");
            }
            shipmentDetails.setMasterBill(consolidationDetails1.getBol());
            shipmentDetails.setDirection(consolidationDetails1.getShipmentType());
            setBookingNumberInShipment(shipmentDetails, consolidationDetails1);
            processCarrierDetailsForShipmentConsole(shipmentDetails, consolidationDetails1);
            processInterBranchConsoleInBeforeSave(shipmentDetails, consolidationDetails1);
            ConsolidationDetails console = shipmentDetails.getConsolidationList().iterator().next();
            if (!Objects.isNull(console) && !Objects.isNull(console.getId()))
                awbDao.validateAirMessaging(console.getId());
            deletePendingRequestsOnConsoleAttach(shipmentDetails, isCreate);
        } else {
            if(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getIsRunnerV3Enabled()) && Boolean.TRUE.equals(isRouteMasterEnabled) && mainCarriageRoutings != null && !mainCarriageRoutings.isEmpty()) {
                    shipmentDetails.getCarrierDetails().setEtd(mainCarriageRoutings.get(0).getEtd());
                    shipmentDetails.getCarrierDetails().setEta(mainCarriageRoutings.get(mainCarriageRoutings.size() - 1).getEta());
                }
        }
    }

    private void setBookingNumberInShipment(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails1) {
        if (consolidationDetails1.getId() != null) {
            Optional<ConsolidationDetails> consol = consolidationDetailsDao.findById(consolidationDetails1.getId());
            if (!consol.isEmpty() && !CommonUtils.isStringNullOrEmpty(consol.get().getBookingNumber())) {
                shipmentDetails.setBookingNumber(consol.get().getBookingNumber());
            } else if(!consol.isEmpty()) {
                shipmentDetails.setBookingNumber(consol.get().getCarrierBookingRef());
            }
        }
    }

    private void processCarrierDetailsForShipmentConsole(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails1) {
        if (shipmentDetails.getCarrierDetails() == null) {
            shipmentDetails.setCarrierDetails(new CarrierDetails());
        }
        if (consolidationDetails1.getCarrierDetails() != null) {
            shipmentDetails.getCarrierDetails().setVoyage(consolidationDetails1.getCarrierDetails().getVoyage());
            shipmentDetails.getCarrierDetails().setVessel(consolidationDetails1.getCarrierDetails().getVessel());
            shipmentDetails.getCarrierDetails().setShippingLine(consolidationDetails1.getCarrierDetails().getShippingLine());
            shipmentDetails.getCarrierDetails().setAircraftType(consolidationDetails1.getCarrierDetails().getAircraftType());
            shipmentDetails.getCarrierDetails().setCfs(consolidationDetails1.getCarrierDetails().getCfs());

            if (Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) {
                shipmentDetails.getCarrierDetails().setFlightNumber(consolidationDetails1.getCarrierDetails().getFlightNumber());
                shipmentDetails.getCarrierDetails().setOriginPort(consolidationDetails1.getCarrierDetails().getOriginPort());
                shipmentDetails.getCarrierDetails().setDestinationPort(consolidationDetails1.getCarrierDetails().getDestinationPort());
                shipmentDetails.getCarrierDetails().setEtd(consolidationDetails1.getCarrierDetails().getEtd());
                shipmentDetails.getCarrierDetails().setEta(consolidationDetails1.getCarrierDetails().getEta());
                shipmentDetails.getCarrierDetails().setAtd(consolidationDetails1.getCarrierDetails().getAtd());
                shipmentDetails.getCarrierDetails().setAta(consolidationDetails1.getCarrierDetails().getAta());
            }
        }
    }

    private void processInterBranchConsoleInBeforeSave(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails1) {
        if(!Boolean.TRUE.equals(consolidationDetails1.getInterBranchConsole())) {
            if (CommonUtils.checkPartyNotNull(consolidationDetails1.getSendingAgent())) {
                setExportBrokerForInterBranchConsole(shipmentDetails, consolidationDetails1);
            }

            if (CommonUtils.checkPartyNotNull(consolidationDetails1.getReceivingAgent())) {
                setImportBrokerForInterBranchConsole(shipmentDetails, consolidationDetails1);
            }
        }
    }

    private Set<Containers> getUpdatedContainers(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, Set<ContainerRequest> containerRequest, Long consolidationId, Long id) throws RunnerException {
        Set<Containers> updatedContainers = new HashSet<>();

        if (containerRequest != null) {
            for (ContainerRequest containerRequest1 : containerRequest) {
                containerRequest1.setConsolidationId(consolidationId);
                if (Boolean.TRUE.equals(containerRequest1.getHazardous()))
                    shipmentDetails.setContainsHazardous(true);
            }
            updatedContainers = new HashSet<>(containerDao.updateEntityFromShipmentConsole(commonUtils.convertToEntityList(new ArrayList<>(containerRequest),
                                              Containers.class, isCreate), consolidationId, id, false));
        } else if (!Objects.isNull(oldEntity)) {
            updatedContainers = oldEntity.getContainersList();
        }
        return updatedContainers;
    }

    private List<Long> processConsolidationDetailsRequests(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Long> removedConsolIds, MutableBoolean isNewConsolAttached, Set<ConsolidationDetailsRequest> consolidationDetailsRequests, List<Long> tempConsolIds) {
        if (consolidationDetailsRequests != null) {
            Set<Long> oldConsolIds = getOldConsolIds(oldEntity, consolidationDetailsRequests, tempConsolIds);
            if (!Objects.isNull(oldConsolIds)) {
                for (Long oldConsoleId : oldConsolIds)
                    removedConsolIds.add(oldConsoleId);
            }

            // Check if the consolidation details are not empty and if one of the following conditions is true:
            // - The old entity is null (no previous data exists).
            // - The old entity's consolidation list is empty (no prior consolidations).
            // - There are removed consolidation IDs (indicating changes in consolidations).
            validateDPSException(shipmentDetails, oldEntity, removedConsolIds, isNewConsolAttached, consolidationDetailsRequests);
        } else {
            shipmentDetails.setConsolRef(null);
            tempConsolIds = Objects.isNull(oldEntity) ? new ArrayList<>() : oldEntity.getConsolidationList().stream().map(e -> e.getId()).toList();
        }
        return tempConsolIds;
    }

    private Set<Long> getOldConsolIds(ShipmentDetails oldEntity, Set<ConsolidationDetailsRequest> consolidationDetailsRequests, List<Long> tempConsolIds) {
        Set<Long> oldConsolIds = Objects.isNull(oldEntity) ? null : oldEntity.getConsolidationList().stream().map(e -> e.getId()).collect(Collectors.toSet());
        for (ConsolidationDetailsRequest consolidation : consolidationDetailsRequests) {
            if (consolidation.getId() != null) {
                tempConsolIds.add(consolidation.getId());
                if (!Objects.isNull(oldConsolIds) && oldConsolIds.contains(consolidation.getId()))
                    oldConsolIds.remove(consolidation.getId());
            }
        }
        return oldConsolIds;
    }

    private void validateDPSException(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Long> removedConsolIds, MutableBoolean isNewConsolAttached, Set<ConsolidationDetailsRequest> consolidationDetailsRequests) {
        if (ObjectUtils.isNotEmpty(consolidationDetailsRequests)
                && (oldEntity == null || ObjectUtils.isEmpty(oldEntity.getConsolidationList()) || ObjectUtils.isNotEmpty(removedConsolIds))) {

            // Check if the specific implication (CONCR) is already present for the given shipment ID.
            // If true, throw a ValidationException to prevent further processing to maintain business constraints.
            if (Objects.nonNull(shipmentDetails.getId()) && Boolean.TRUE.equals(dpsEventService.isImplicationPresent(List.of(shipmentDetails.getId()), DpsConstants.CONCR))) {
                throw new ValidationException(DpsConstants.DPS_ERROR_1);
            }

            isNewConsolAttached.setTrue();
        }
    }

    private boolean getUpdatedSyncConsole(ShipmentDetails shipmentDetails, boolean isCreate, ShipmentRequest shipmentRequest, ShipmentSettingsDetails shipmentSettingsDetails, Set<Containers> updatedContainers, List<Long> tempConsolIds, boolean syncConsole) throws RunnerException {
        ConsolidationDetails consolidationDetails;
        if((!updatedContainers.isEmpty() || (shipmentRequest.getAutoCreateConsole() != null  && shipmentRequest.getAutoCreateConsole())) && (tempConsolIds == null || tempConsolIds.isEmpty()) && (shipmentSettingsDetails.getIsShipmentLevelContainer() == null || !shipmentSettingsDetails.getIsShipmentLevelContainer())) {

            // Check if the specific implication (CONCR) is already present for the given shipment ID.
            // If true, throw a ValidationException to prevent further processing.
            if (Objects.nonNull(shipmentDetails.getId()) && Boolean.TRUE.equals(dpsEventService.isImplicationPresent(List.of(shipmentDetails.getId()), DpsConstants.CONCR))) {
                throw new ValidationException(DpsConstants.DPS_ERROR_1);
            }

            deletePendingRequestsOnConsoleAttach(shipmentDetails, isCreate);
            consolidationDetails = createConsolidation(shipmentDetails, new ArrayList<>(updatedContainers));
            if (!Objects.isNull(consolidationDetails)) {
                shipmentDetails.setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails)));
                if (isStringNullOrEmpty(shipmentDetails.getMasterBill()))
                    shipmentDetails.setMasterBill(consolidationDetails.getBol());
                syncConsole = true;
            }
        }
        return syncConsole;
    }

    private void validateStaleShipmentUpdateError(ShipmentDetails shipmentDetails, boolean isCreate) {
        if(!isCreate) {
            // Check the shipment for attached consolidation, if the user is updating stale shipment and causing shipment to detach
            List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByShipmentId(shipmentDetails.getId());
            if (!CollectionUtils.isEmpty(consoleShipmentMappings)) {
                consoleShipmentMappings = consoleShipmentMappings.stream().filter(i -> Boolean.TRUE.equals(i.getIsAttachmentDone())).toList();
                if (CollectionUtils.isEmpty(shipmentDetails.getConsolidationList()) && !consoleShipmentMappings.isEmpty()
                        && !Objects.isNull(consoleShipmentMappings.get(0).getRequestedType())) {
                    throw new ValidationException(ShipmentConstants.STALE_SHIPMENT_UPDATE_ERROR);
                }
            }
        }
    }

    private boolean shouldSetPorts(ShipmentRequest shipmentRequest) {
        return (shipmentRequest.getShipmentType().equals(SHIPMENT_TYPE_HSE) && Boolean.FALSE.equals(shipmentRequest.getB2b())) ||
                shipmentRequest.getShipmentType().equals(SHIPMENT_TYPE_SCN) ||
                shipmentRequest.getShipmentType().equals(SHIPMENT_TYPE_BCN) ||
                shipmentRequest.getShipmentType().equals(SHIPMENT_TYPE_DRT) ||
                Boolean.TRUE.equals(shipmentRequest.getB2b());
    }

    private CompletableFuture<Void> getPopulateUnlocCodeFuture(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        CarrierDetails finalOldCarrierDetails = Optional.ofNullable(oldEntity).map(ShipmentDetails::getCarrierDetails).orElse(null);
        List<Routings> finalOldRoutings = Optional.ofNullable(oldEntity).map(ShipmentDetails::getRoutingsList).orElse(Collections.emptyList());

        /* Set to extract the unlocations from entities whose unloc code needs to be saved */
        Set<String> unlocationsSet = Collections.synchronizedSet(new HashSet<>());
        Map<String, EntityTransferUnLocations> unLocationsMap = new ConcurrentHashMap<>();

        CompletableFuture.allOf(
                CompletableFuture.runAsync(() -> commonUtils.getChangedUnLocationFields(shipmentDetails.getCarrierDetails(), finalOldCarrierDetails, unlocationsSet), executorService),
                CompletableFuture.runAsync(() -> commonUtils.getChangedUnLocationFields(shipmentDetails.getRoutingsList(), finalOldRoutings, unlocationsSet), executorService)
        ).join();

        return CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.getLocationDataFromCache(unlocationsSet, unLocationsMap)), executorService)
                .thenCompose(v -> CompletableFuture.allOf(
                    CompletableFuture.runAsync(() -> commonUtils.updateCarrierUnLocData(shipmentDetails.getCarrierDetails(), unLocationsMap), executorService),
                    CompletableFuture.runAsync(() -> commonUtils.updateRoutingUnLocData(shipmentDetails.getRoutingsList(), unLocationsMap), executorService)
        ));
    }

    private void deletePendingRequestsOnConsoleAttach(ShipmentDetails shipmentDetails, boolean isCreate) {
        if(!isCreate) {
            ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, shipmentDetails.getId(), "=", null);
            listCommonRequest = andCriteria("isAttachmentDone", false, "=", listCommonRequest);
            Pair<Specification<ConsoleShipmentMapping>, Pageable> pair = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
            List<ConsoleShipmentMapping> consoleShipmentMappingsForEmails = jsonHelper.convertValueToList(consoleShipmentMappingDao.findAll(pair.getLeft(), pair.getRight()).getContent(), ConsoleShipmentMapping.class);
            if(!listIsNullOrEmpty(consoleShipmentMappingsForEmails)) {
                consoleShipmentMappingDao.deletePendingStateByShipmentId(shipmentDetails.getId());
                List<Long> otherConsoleIds = consoleShipmentMappingsForEmails.stream().map(e -> e.getConsolidationId()).toList();
                List<ConsolidationDetails> otherConsolidationDetails = consolidationDetailsDao.findConsolidationsByIds(new HashSet<>(otherConsoleIds));
                commonUtils.sendRejectionEmailsExplicitly(List.of(shipmentDetails), consoleShipmentMappingsForEmails, new HashSet<>(), otherConsolidationDetails);
            }
        }
    }

    public void changeContainerWtVolOnDetach(ShipmentRequest shipmentRequest, List<Containers> allConsolConts) throws RunnerException {
        Map<Long, List<PackingRequest>> containerPacksMap = new HashMap<>();
        getContPacksMap(shipmentRequest, containerPacksMap);
        for(Containers container: allConsolConts) {
            if(CARGO_TYPE_FCL.equals(shipmentRequest.getShipmentType())) {
                containerService.changeContainerWtVolForSeaFCLDetach(container);
            } else {
                if(containerPacksMap.containsKey(container.getId())) {
                    List<PackingRequest> packs = containerPacksMap.get(container.getId());
                    for(PackingRequest packing : packs) {
                        containerService.changeContainerWtVolForSeaLCLDetach(container, jsonHelper.convertValue(packing, Packing.class));
                    }
                }
            }
        }
        containerDao.saveAll(allConsolConts);
    }

    private void getContPacksMap(ShipmentRequest shipmentRequest, Map<Long, List<PackingRequest>> containerPacksMap) {
        if(!listIsNullOrEmpty(shipmentRequest.getPackingList())) {
            for(PackingRequest packing: shipmentRequest.getPackingList()) {
                if(packing.getContainerId() != null) {
                    if(containerPacksMap.containsKey(packing.getContainerId()))
                        containerPacksMap.get(packing.getContainerId()).add(packing);
                    else
                        containerPacksMap.put(packing.getContainerId(), new ArrayList<>(Collections.singletonList(packing)));
                    packing.setContainerId(null);
                }
            }
        }
    }

    public void dgValidations(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails1, int isNewConsoleAttached) throws RunnerException {
        if( ((Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails1.getTransportMode()) && SHIPMENT_TYPE_LCL.equals(consolidationDetails1.getContainerCategory()))
                || checkForAirDGFlag(consolidationDetails1))
                && (Boolean.TRUE.equals(consolidationDetails1.getHazardous()) || Boolean.TRUE.equals(shipmentDetails.getContainsHazardous()))) {
            List<ConsoleShipmentMapping> consoleShipmentMapping = consoleShipmentMappingDao.findByConsolidationId(consolidationDetails1.getId());
            if(!listIsNullOrEmpty(consoleShipmentMapping) && consoleShipmentMapping.size() + isNewConsoleAttached > 1) {
                throwErrorMaxOneShipmentAllowedInDgConsolidation(consolidationDetails1, isNewConsoleAttached);
            }
        }
    }

    private void throwErrorMaxOneShipmentAllowedInDgConsolidation(ConsolidationDetails consolidationDetails1, int isNewConsoleAttached) throws RunnerException {
        if(Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails1.getTransportMode()))
            throw new RunnerException("For Ocean DG Consolidation LCL Cargo Type, and can have only 1 shipment");
        else {
            if(isNewConsoleAttached == 1)
                throw new RunnerException(String.format(CAN_NOT_ATTACH_MORE_SHIPMENTS_IN_DG_CONSOL, consolidationDetails1.getConsolidationNumber()));
            else
                throw new RunnerException(CAN_NOT_UPDATE_DG_SHIPMENTS_CONSOLE_CONSISTS_MULTIPLE_SHIPMENTS);
        }
    }

    public void airDGValidations(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Long> removedConsolIds,
                                    MutableBoolean isNewConsolAttached, Set<ConsolidationDetailsRequest> consolidationDetailsRequests) throws RunnerException {
        if(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag()) && !isAirDgUser()) {
            if(Boolean.TRUE.equals(shipmentDetails.getContainsHazardous())) {
                if(!listIsNullOrEmpty(removedConsolIds) || Boolean.TRUE.equals(isNewConsolAttached.getValue()))
                    throw new RunnerException("You do not have Air DG permissions to attach or detach consolidation as it is a DG Shipment");
            } else {
                if((!listIsNullOrEmpty(removedConsolIds) && oldEntity != null && oldEntity.getConsolidationList() != null && Boolean.TRUE.equals(oldEntity.getConsolidationList().iterator().next().getHazardous()))
                        || (!setIsNullOrEmpty(consolidationDetailsRequests) && Boolean.TRUE.equals(consolidationDetailsRequests.iterator().next().getHazardous()))) {
                    throw new RunnerException("You do not have Air DG permissions to edit this as it is a part of DG Consol");
                }
            }
        }
    }

    public boolean checkIfLCLConsolidationEligible(ShipmentDetails shipmentDetails) {
        if(!Constants.TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode()))
            return false;
        if(!Constants.DIRECTION_EXP.equals(shipmentDetails.getDirection()))
            return false;
        if(!Constants.SHIPMENT_TYPE_LCL.equals(shipmentDetails.getShipmentType()))
            return false;
        return Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableLclConsolidation());
    }

    private void updateShipmentGateInDateAndStatusFromPacks(List<PackingRequest> packingRequests, ShipmentDetails shipmentDetails) throws RunnerException {
        shipmentDetails.setShipmentPackStatus(null);
        if(packingRequests != null && !packingRequests.isEmpty()) {
            shipmentDetails.setShipmentPackStatus(ShipmentPackStatus.BOOKED);
            processPackingRequests(packingRequests, shipmentDetails);
        }
        setShipmentPackStatusSailed(shipmentDetails);
        validateShipmentGateInDate(shipmentDetails);
    }

    private void processPackingRequests(List<PackingRequest> packingRequests, ShipmentDetails shipmentDetails) {
        boolean fullGated = true;
        boolean partialGated = false;
        boolean fullAssigned = true;
        boolean partialAssigned = false;
        LocalDateTime maxDate = null;
        for (PackingRequest packingRequest: packingRequests) {
            if(packingRequest.getCargoGateInDate() != null) {
                if(ACTUAL.equals(packingRequest.getDateType()))
                    partialGated = true;
                else
                    fullGated = false;
                maxDate = getMaxDate(shipmentDetails, packingRequest, maxDate);
            }
            else
                fullGated = false;
            if(packingRequest.getContainerId() != null)
                partialAssigned = true;
            else
                fullAssigned = false;
        }
        setShipmentPackStatusOnAssigned(shipmentDetails, partialAssigned, fullAssigned, partialGated, fullGated);
    }

    private LocalDateTime getMaxDate(ShipmentDetails shipmentDetails, PackingRequest packingRequest, LocalDateTime maxDate) {
        if(maxDate == null || packingRequest.getCargoGateInDate().isAfter(maxDate)) {
            shipmentDetails.setShipmentGateInDate(packingRequest.getCargoGateInDate());
            shipmentDetails.setDateType(packingRequest.getDateType());
            maxDate = packingRequest.getCargoGateInDate();
        }
        return maxDate;
    }

    private void setShipmentPackStatusSailed(ShipmentDetails shipmentDetails) {
        if(shipmentDetails.getCarrierDetails() != null && shipmentDetails.getCarrierDetails().getAtd() != null)
            shipmentDetails.setShipmentPackStatus(SAILED);
    }

    private void validateShipmentGateInDate(ShipmentDetails shipmentDetails) throws RunnerException {
        if(shipmentDetails.getShipmentGateInDate() != null) {
            if(shipmentDetails.getConsolidationList() != null && !shipmentDetails.getConsolidationList().isEmpty()
                    && shipmentDetails.getConsolidationList().iterator().next().getCfsCutOffDate() != null) {
                if(shipmentDetails.getShipmentGateInDate().isAfter(shipmentDetails.getConsolidationList().iterator().next().getCfsCutOffDate()))
                    throw new RunnerException("Shipment Gate In date should not be greater than the CFS Cut Off Date entered at the consolidation level.");
            }
            else if(shipmentDetails.getCarrierDetails().getEtd() != null && shipmentDetails.getShipmentGateInDate().isAfter(shipmentDetails.getCarrierDetails().getEtd()))
                throw new RunnerException("Shipment Gate In Date cannot be greater than ETD.");
        }
    }

    private void setShipmentPackStatusOnAssigned(ShipmentDetails shipmentDetails, boolean partialAssigned, boolean fullAssigned, boolean partialGated, boolean fullGated) {
        if(partialAssigned)
            shipmentDetails.setShipmentPackStatus(ShipmentPackStatus.PARTIALLY_ASSIGNED);
        if(fullAssigned)
            shipmentDetails.setShipmentPackStatus(ShipmentPackStatus.ASSIGNED);
        if(partialGated)
            shipmentDetails.setShipmentPackStatus(ShipmentPackStatus.PARTIAL_CARGO_GATE_IN);
        if(fullGated)
            shipmentDetails.setShipmentPackStatus(ShipmentPackStatus.CARGO_GATED_IN);
    }

    private boolean checkOriginalPrintedForJobTypeChange(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if(oldEntity == null)
            return false;
        if(!Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_SEA))
            return false;
        if(!Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP))
            return false;
        if(!Boolean.TRUE.equals(shipmentDetails.getAdditionalDetails().getPrintedOriginal()))
            return false;
        return !Objects.equals(shipmentDetails.getJobType(), oldEntity.getJobType());
    }

    private boolean checkDisableFetchConditionForAwb(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity,ShipmentSettingsDetails shipmentSettingsDetails){
        if(oldEntity == null)
            return false;
        if(!Boolean.TRUE.equals(shipmentSettingsDetails.getIataTactFlag()))
            return false;
        if(!Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR))
            return false;
        if(!Objects.equals(shipmentDetails.getJobType(), Constants.SHIPMENT_TYPE_DRT))
            return false;
        return !Objects.equals(shipmentDetails.getCarrierDetails().getOriginPort(), oldEntity.getCarrierDetails().getOriginPort()) || !Objects.equals(shipmentDetails.getCarrierDetails().getDestinationPort(), oldEntity.getCarrierDetails().getDestinationPort())
                || !Objects.equals(shipmentDetails.getCarrierDetails().getShippingLine(), oldEntity.getCarrierDetails().getShippingLine());
    }

    private void validateBeforeSave(ShipmentDetails shipmentDetails) {
        if(shipmentDetails.getConsignee() != null && shipmentDetails.getConsigner() != null && shipmentDetails.getConsignee().getOrgCode() != null && shipmentDetails.getConsigner().getOrgCode() != null && shipmentDetails.getConsigner().getOrgCode().equals(shipmentDetails.getConsignee().getOrgCode()))
            throw new ValidationException("Consignor & Consignee parties can't be selected as same.");

        if(!isStringNullOrEmpty(shipmentDetails.getJobType()) && shipmentDetails.getJobType().equals(Constants.SHIPMENT_TYPE_DRT)){
            if(!isStringNullOrEmpty(shipmentDetails.getTransportMode()) && !shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && !shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                shipmentDetails.setHouseBill(shipmentDetails.getMasterBill());
            }
            else if(!isStringNullOrEmpty(shipmentDetails.getTransportMode()) && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) ||
                    shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA))) {
                shipmentDetails.setHouseBill(null);
            }
        }

        if(!Objects.isNull(shipmentDetails.getConsolidationList()) && !shipmentDetails.getConsolidationList().isEmpty()) {
            shipmentDetails.setConsolRef(shipmentDetails.getConsolidationList().iterator().next().getReferenceNumber());
        }

    }

    public void validateRaKcDetails(ShipmentDetails shipmentDetails, V1TenantSettingsResponse tenantSettingsResponse) throws RunnerException {
        // bypass all RA/KC validations in case of AMR air freight
        if(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getIsAmrAirFreightEnabled()))
            return;

        Parties consignor = shipmentDetails.getConsigner();
        if(Boolean.TRUE.equals(tenantSettingsResponse.getEnableAirMessaging()) && Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) {
            List<Parties> orgList = new ArrayList<>();
            if(consignor != null && StringUtility.isNotEmpty(consignor.getAddressCode())) {
                    orgList.add(consignor);
                }

            if(shipmentDetails.getId() != null && shipmentDetails.getAdditionalDetails() != null && shipmentDetails.getAdditionalDetails().getExportBroker() != null && StringUtility.isNotEmpty(shipmentDetails.getAdditionalDetails().getExportBroker().getAddressCode())) {
                    orgList.add(shipmentDetails.getAdditionalDetails().getExportBroker());
            }

            processOrgList(shipmentDetails, orgList, consignor);
        }
    }

    private void processOrgList(ShipmentDetails shipmentDetails, List<Parties> orgList, Parties consignor) throws RunnerException {
        if(!orgList.isEmpty()) {
            OrgAddressResponse orgAddressResponse = v1ServiceUtil.fetchOrgInfoFromV1(orgList);
            if (orgAddressResponse != null) {
                Map<String, Map<String, Object>> addressMap = orgAddressResponse.getAddresses();
                if (anyPartyExpire(addressMap)) return;
                validateConsignorScreeningAndSecurity(shipmentDetails, consignor, addressMap);

                validateScreeningAndSecurityForRa(shipmentDetails, orgAddressResponse);

            }
        }
    }

    private boolean anyPartyExpire(Map<String, Map<String, Object>> addressMap) {
        int countOfExpiredParties = 0;
        int countOfShipmentRaKcParties = 0;
        for(var entry : addressMap.entrySet()) {
            if (entry.getValue() != null && StringUtility.isNotEmpty(StringUtility.convertToString(entry.getValue().get(KCRA_EXPIRY)))) {
                LocalDateTime agentExpiry = LocalDateTime.parse(StringUtility.convertToString(entry.getValue().get(KCRA_EXPIRY)));
                // if any one of the agent is not expired will apply the validations as is
                countOfShipmentRaKcParties++;
                if (LocalDateTime.now().isAfter(agentExpiry))
                    countOfExpiredParties++;
            }
        }
        boolean isPartyExpire = false;
        if(countOfExpiredParties == countOfShipmentRaKcParties && countOfExpiredParties > 0)
            isPartyExpire = true;
        return isPartyExpire;
    }

    private void validateConsignorScreeningAndSecurity(ShipmentDetails shipmentDetails, Parties consignor, Map<String, Map<String, Object>> addressMap) throws RunnerException {
        if(consignor != null && addressMap.containsKey(consignor.getOrgCode() + "#" + consignor.getAddressCode())) {
            Map<String, Object> addressConsignorAgent = addressMap.get(consignor.getOrgCode() + "#" + consignor.getAddressCode());
            if (addressConsignorAgent.containsKey(Constants.KNOWN_CONSIGNOR)) {
                var rakcType = addressConsignorAgent.get(Constants.KNOWN_CONSIGNOR);
                if (rakcType != null && Boolean.TRUE.equals(rakcType) && (shipmentDetails.getAdditionalDetails().getScreeningStatus() == null ||
                        shipmentDetails.getAdditionalDetails().getScreeningStatus().isEmpty() ||
                        shipmentDetails.getSecurityStatus() == null)) {
                    throw new RunnerException("Screening Status and Security Status is mandatory for KC consginor.");
                }
                else if(shipmentDetails.getAdditionalDetails().getScreeningStatus() != null && shipmentDetails.getAdditionalDetails().getScreeningStatus().size() == 1 && shipmentDetails.getAdditionalDetails().getScreeningStatus().get(0).equals("VCK")) {
                    throw new ValidationException("Please select an additional screening status along with VCK.");
                }
            }
        }
    }

    private void validateScreeningAndSecurityForRa(ShipmentDetails shipmentDetails, OrgAddressResponse orgAddressResponse) throws RunnerException {
        if(shipmentDetails.getId() != null && shipmentDetails.getAdditionalDetails() != null && shipmentDetails.getAdditionalDetails().getExportBroker() != null && StringUtility.isNotEmpty(shipmentDetails.getAdditionalDetails().getExportBroker().getAddressCode()) && !checkRaStatusFields(shipmentDetails, orgAddressResponse, shipmentDetails.getAdditionalDetails().getExportBroker())) {
            throw new RunnerException("Screening Status and Security Status is mandatory for RA Origin Agent.");
        }
    }

    public boolean checkRaStatusFields(ShipmentDetails shipmentDetails, OrgAddressResponse orgAddressResponse, Parties parties) throws ValidationException{
        Map<String, Map<String, Object>> addressMap = orgAddressResponse.getAddresses();
        if (addressMap.containsKey(parties.getOrgCode() + "#" + parties.getAddressCode())) {
            Map<String, Object> addressConsignorAgent = addressMap.get(parties.getOrgCode() + "#" + parties.getAddressCode());
            if (addressConsignorAgent.containsKey(Constants.REGULATED_AGENT)) {
                var rakcType = addressConsignorAgent.get(Constants.REGULATED_AGENT);
                if (rakcType != null && Boolean.TRUE.equals(rakcType)){
                    if(shipmentDetails.getAdditionalDetails().getScreeningStatus() == null ||
                        shipmentDetails.getAdditionalDetails().getScreeningStatus().isEmpty() ||
                        shipmentDetails.getSecurityStatus() == null){
                        return false;
                    }
                    else if(shipmentDetails.getAdditionalDetails().getScreeningStatus() != null && shipmentDetails.getAdditionalDetails().getScreeningStatus().size() == 1 && shipmentDetails.getAdditionalDetails().getScreeningStatus().get(0).equals("VCK")){
                        throw new ValidationException("Please select an additional screening status along with VCK.");
                    }
                }
            }
        }
        return true;
    }

    public void afterSave(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, ShipmentRequest shipmentRequest, ShipmentSettingsDetails shipmentSettingsDetails, boolean syncConsole, List<Long> removedConsolIds, MutableBoolean isNewConsolAttached, boolean includeGuid, boolean isFromET) throws RunnerException {
        log.info("shipment afterSave start.... ");
        List<BookingCarriageRequest> bookingCarriageRequestList = shipmentRequest.getBookingCarriagesList();
        List<TruckDriverDetailsRequest> truckDriverDetailsRequestList = shipmentRequest.getTruckDriverDetails();
        List<PackingRequest> packingRequestList = shipmentRequest.getPackingList();
        List<ELDetailsRequest> elDetailsRequestList = shipmentRequest.getElDetailsList();
        List<EventsRequest> eventsRequestList = shipmentRequest.getEventsList();
        List<NotesRequest> notesRequestList = shipmentRequest.getNotesList();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = shipmentRequest.getReferenceNumbersList();
        List<RoutingsRequest> routingsRequestList = jsonHelper.convertValueToList(shipmentDetails.getRoutingsList(), RoutingsRequest.class);
        List<ServiceDetailsRequest> serviceDetailsRequestList = shipmentRequest.getServicesList();
        List<PartiesRequest> shipmentAddressList = shipmentRequest.getShipmentAddresses();
        List<PickupDeliveryDetailsRequest> pickupDeliveryDetailsRequests = shipmentRequest.getPickupDeliveryDetailsInstructions();
        List<ShipmentOrderRequest> shipmentOrderRequestList = shipmentRequest.getShipmentOrders();
        log.info("shipment afterSave request build.... ");

        storeMblAudit(shipmentDetails, oldEntity);
        log.info("shipment afterSave mblcheck.... ");

        Long id = shipmentDetails.getId();
        Long consolidationId = getConsolidationIdFromShipment(shipmentDetails);

        Set<Containers> updatedContainers = shipmentDetails.getContainersList();
        List<Packing> updatedPackings = new ArrayList<>();
        List<Long> deleteContainerIds = new ArrayList<>();
        List<Packing> packsForSync = null;
        List<UUID> deletedContGuids = new ArrayList<>();

        if (!isCreate){
            if (shipmentRequest.getDeletedContainerIds() != null && !shipmentRequest.getDeletedContainerIds().isEmpty()) {
                deleteContainerIds = shipmentRequest.getDeletedContainerIds().stream().filter(e -> e.getId() != null).map(e -> e.getId()).toList();
                if (deleteContainerIds != null && !deleteContainerIds.isEmpty()) {
                    packsForSync = getPacksForSync(deleteContainerIds, packsForSync);
                    deletedContGuids = getDeletedContGuids(deleteContainerIds, deletedContGuids);
                    containerDao.deleteByIdIn(deleteContainerIds);
                }
            }

            // Update AWB
            updateAwb(shipmentDetails, oldEntity);
        }
        log.info("shipment afterSave isCreate .... ");
        shipmentRequest.setId(id);
        dateTimeChangeLogService.createEntryFromShipment(shipmentRequest, oldEntity);
        log.info("shipment afterSave dateTimeChangeLogService .... ");
        if (bookingCarriageRequestList != null) {
            List<BookingCarriage> updatedBookingCarriages = bookingCarriageDao.updateEntityFromShipment(commonUtils.convertToEntityList(bookingCarriageRequestList, BookingCarriage.class, isCreate), id);
            shipmentDetails.setBookingCarriagesList(updatedBookingCarriages);
        }
        log.info("shipment afterSave bookingCarriageDao.... ");
        if (truckDriverDetailsRequestList != null) {
            List<TruckDriverDetails> updatedTruckDriverDetails = truckDriverDetailsDao.updateEntityFromShipment(commonUtils.convertToEntityList(truckDriverDetailsRequestList, TruckDriverDetails.class, isCreate), id);
            shipmentDetails.setTruckDriverDetails(updatedTruckDriverDetails);
        }
        log.info("shipment afterSave truckDriverDetailsDao.... ");
        if (elDetailsRequestList != null) {
            List<ELDetails> updatedELDetails = elDetailsDao.updateEntityFromShipment(commonUtils.convertToEntityList(elDetailsRequestList, ELDetails.class, isCreate), id);
            shipmentDetails.setElDetailsList(updatedELDetails);
        }
        log.info("shipment afterSave elDetailsDao.... ");
        ConsolidationDetails consolidationDetails = updateLinkedShipmentData(shipmentDetails, oldEntity, shipmentRequest);
        log.info("shipment afterSave updateLinkedShipmentData.... ");
        if(!Objects.isNull(consolidationDetails)) {
            shipmentDetails.setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails)));
            syncConsole = true;
        }

        // make removed consolidation non dg if all other shipments are non dg
        consolidationDetails = makeConsoleNonDg(shipmentDetails, oldEntity, isCreate, removedConsolIds, consolidationDetails);
        log.info("shipment afterSave DG Check.... ");

        // Sci status update for attach and detach in console mawb
        checkSciForAttachDetachConsole(shipmentDetails, removedConsolIds, isNewConsolAttached, consolidationId);
        log.info("shipment afterSave checkSciForAttachConsole.... ");
        processEventsInAfterSave(shipmentDetails, oldEntity, isCreate, shipmentSettingsDetails, eventsRequestList, id);

        updatedPackings = getUpdatedPackingList(shipmentDetails, isCreate, includeGuid, packingRequestList, consolidationId, updatedPackings, id, deleteContainerIds);
        processListRequests(shipmentDetails, isCreate, referenceNumbersRequestList, id, routingsRequestList, serviceDetailsRequestList, notesRequestList, shipmentAddressList, pickupDeliveryDetailsRequests, shipmentOrderRequestList);
        processReplaceConsoleRoute(shipmentRequest);
        log.info("shipment afterSave createShipmentRouteInConsole..... ");
        Hbl hbl = getHblInAfterSave(shipmentDetails, updatedContainers, updatedPackings);
        log.info("shipment afterSave hblService.checkAllContainerAssigned..... ");
        pushShipmentDataToDependentService(shipmentDetails, oldEntity, isCreate, shipmentRequest, isFromET);

        if(!Objects.isNull(shipmentDetails.getConsolidationList()) && !shipmentDetails.getConsolidationList().isEmpty()){
            consolidationDetails = shipmentDetails.getConsolidationList().iterator().next();
        }

        // Delete the shipment pending pull/push request tasks when the shipment got cancelled
        deletePendingStateAfterCancellation(shipmentDetails, oldEntity);
        log.info("shipment afterSave consoleShipmentMappingDao.deletePendingStateByShipmentId..... ");
        processSyncV1AndAsyncFunctions(shipmentDetails, oldEntity, shipmentSettingsDetails, syncConsole, hbl, deletedContGuids, packsForSync, consolidationDetails);
        log.info("shipment afterSave end..... ");
    }

    private void updateAwb(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) throws RunnerException {
        if(checkForAwbUpdate(shipmentDetails, oldEntity)) {
            awbDao.updatedAwbInformationEvent(shipmentDetails, oldEntity);
        }
    }

    private Long getConsolidationIdFromShipment(ShipmentDetails shipmentDetails) {
        Long consolidationId = null;
        if(shipmentDetails.getConsolidationList() != null && !shipmentDetails.getConsolidationList().isEmpty())
            consolidationId = shipmentDetails.getConsolidationList().iterator().next().getId();
        return consolidationId;
    }

    private void deletePendingStateAfterCancellation(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if (Boolean.TRUE.equals(commonUtils.getCurrentTenantSettings().getIsMAWBColoadingEnabled()) && Objects.nonNull(oldEntity)
                && !Objects.equals(oldEntity.getStatus(), shipmentDetails.getStatus()) && Objects.equals(shipmentDetails.getStatus(), ShipmentStatus.Cancelled.getValue())) {
            log.info("Request: {} | Deleting console_shipment_mapping due to shipment cancelled for shipment: {}", LoggerHelper.getRequestIdFromMDC(), shipmentDetails.getShipmentId());
            consoleShipmentMappingDao.deletePendingStateByShipmentId(shipmentDetails.getId());
        }
    }

    private void processReplaceConsoleRoute(ShipmentRequest shipmentRequest) throws RunnerException {
        if (shipmentRequest.getReplaceConsoleRoute() != null && shipmentRequest.getReplaceConsoleRoute()){
            createShipmentRouteInConsole(shipmentRequest);
        }
    }

    private List<Packing> getUpdatedPackingList(ShipmentDetails shipmentDetails, boolean isCreate, boolean includeGuid, List<PackingRequest> packingRequestList, Long consolidationId, List<Packing> updatedPackings, Long id, List<Long> deleteContainerIds) throws RunnerException {
        if (packingRequestList != null) {
            packingRequestList = setPackingDetails(packingRequestList, shipmentDetails.getTransportMode(), consolidationId);
            updatedPackings = packingDao.updateEntityFromShipment(commonUtils.convertToEntityList(packingRequestList, Packing.class, !includeGuid && isCreate), id, deleteContainerIds);
            shipmentDetails.setPackingList(updatedPackings);
        }
        log.info("shipment afterSave packingDao.updateEntityFromShipment..... ");
        return updatedPackings;
    }

    private void processListRequests(ShipmentDetails shipmentDetails, boolean isCreate, List<ReferenceNumbersRequest> referenceNumbersRequestList, Long id, List<RoutingsRequest> routingsRequestList, List<ServiceDetailsRequest> serviceDetailsRequestList, List<NotesRequest> notesRequestList, List<PartiesRequest> shipmentAddressList, List<PickupDeliveryDetailsRequest> pickupDeliveryDetailsRequests, List<ShipmentOrderRequest> shipmentOrderRequestList) throws RunnerException {
        if (referenceNumbersRequestList != null) {
            List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(commonUtils.convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class, isCreate), id);
            shipmentDetails.setReferenceNumbersList(updatedReferenceNumbers);
        }
        log.info("shipment afterSave referenceNumbersDao.updateEntityFromShipment..... ");
        if (routingsRequestList != null) {
            List<Routings> updatedRoutings = routingsDao.updateEntityFromShipment(commonUtils.convertToEntityList(routingsRequestList, Routings.class, isCreate), id);
            shipmentDetails.setRoutingsList(updatedRoutings);
        }
        log.info("shipment afterSave routingsDao.updateEntityFromShipment..... ");
        if (serviceDetailsRequestList != null) {
            List<ServiceDetails> updatedServiceDetails = serviceDetailsDao.updateEntityFromShipment(commonUtils.convertToEntityList(serviceDetailsRequestList, ServiceDetails.class, isCreate), id);
            shipmentDetails.setServicesList(updatedServiceDetails);
        }
        log.info("shipment afterSave serviceDetailsDao.updateEntityFromShipment..... ");
        if (notesRequestList != null) {
            List<Notes> updatedNotes = notesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(notesRequestList, Notes.class, isCreate), id, Constants.SHIPMENT);
            shipmentDetails.setNotesList(updatedNotes);
        }
        log.info("shipment afterSave partiesDao.updateEntityFromOtherEntity..... ");
        if (shipmentAddressList != null) {
            List<Parties> updatedParties = partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(shipmentAddressList, Parties.class, isCreate), id, Constants.SHIPMENT_ADDRESSES);
            shipmentDetails.setShipmentAddresses(updatedParties);
        }
        log.info("shipment afterSave partiesDao.updateEntityFromOtherEntity..... ");
        if (pickupDeliveryDetailsRequests != null){
            List<PickupDeliveryDetails> pickupDeliveryDetailsList = pickupDeliveryDetailsDao.updateEntityFromShipment(commonUtils.convertToEntityList(pickupDeliveryDetailsRequests, PickupDeliveryDetails.class , isCreate) , id);
            shipmentDetails.setPickupDeliveryDetailsInstructions(pickupDeliveryDetailsList);
        }
        log.info("shipment afterSave pickupDeliveryDetailsDao.updateEntityFromShipment..... ");
        if(shipmentOrderRequestList != null) {
            List<ShipmentOrder> shipmentOrders = shipmentOrderDao.updateEntityFromShipment(jsonHelper.convertValueToList(shipmentOrderRequestList, ShipmentOrder.class), id);
            shipmentDetails.setShipmentOrders(shipmentOrders);
        }
        log.info("shipment afterSave shipmentOrderDao.updateEntityFromShipment..... ");
    }

    private void pushShipmentDataToDependentService(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, ShipmentRequest shipmentRequest, boolean isFromET) {
        if(!isFromET) {
            dependentServiceHelper.pushShipmentDataToDependentService(shipmentDetails, isCreate, Boolean.TRUE.equals(shipmentRequest.getIsAutoSellRequired()), Optional.ofNullable(oldEntity).map(ShipmentDetails::getContainersList).orElse(null));
            log.info("shipment afterSave pushShipmentDataToDependentService..... ");
        }
    }

    private Hbl getHblInAfterSave(ShipmentDetails shipmentDetails, Set<Containers> updatedContainers, List<Packing> updatedPackings) {
        Hbl hbl = null;
        if(updatedContainers != null && !updatedContainers.isEmpty()) {
            hbl = hblService.checkAllContainerAssigned(shipmentDetails, updatedContainers, updatedPackings);
        }
        return hbl;
    }

    private void checkSciForAttachDetachConsole(ShipmentDetails shipmentDetails, List<Long> removedConsolIds, MutableBoolean isNewConsolAttached, Long consolidationId) throws RunnerException {
        if(removedConsolIds != null && !removedConsolIds.isEmpty() && Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)){
            consolidationService.checkSciForDetachConsole(removedConsolIds.get(0));
        }
        log.info("shipment afterSave checkSciForDetachConsole.... ");
        if(Boolean.TRUE.equals(isNewConsolAttached.getValue()) && Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) {
            consolidationService.checkSciForAttachConsole(consolidationId);
        }
    }

    private void processSyncV1AndAsyncFunctions(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, ShipmentSettingsDetails shipmentSettingsDetails, boolean syncConsole, Hbl hbl, List<UUID> deletedContGuids, List<Packing> packsForSync, ConsolidationDetails consolidationDetails) {
        // Syncing shipment to V1
        syncShipment(shipmentDetails, hbl, deletedContGuids, packsForSync, consolidationDetails, syncConsole);
        log.info("shipment afterSave syncShipment..... ");
        if (commonUtils.getCurrentTenantSettings().getP100Branch() != null && commonUtils.getCurrentTenantSettings().getP100Branch())
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> bookingIntegrationsUtility.updateBookingInPlatform(shipmentDetails)), executorService);
        if(Boolean.TRUE.equals(shipmentSettingsDetails.getIsNetworkTransferEntityEnabled()))
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> createOrUpdateNetworkTransferEntity(shipmentDetails, oldEntity)), executorService);
        if(Boolean.TRUE.equals(shipmentSettingsDetails.getIsAutomaticTransferEnabled()))
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> triggerAutomaticTransfer(shipmentDetails, oldEntity, false)), executorService);
    }

    private void processEventsInAfterSave(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, ShipmentSettingsDetails shipmentSettingsDetails, List<EventsRequest> eventsRequestList, Long id) throws RunnerException {
        if (eventsRequestList != null) {
            eventsRequestList = setEventDetails(eventsRequestList, shipmentDetails);
            List<Events> eventsList = commonUtils.convertToEntityList(eventsRequestList, Events.class, isCreate);
            eventsList = createOrUpdateEvents(shipmentDetails, oldEntity, eventsList, isCreate);
            if (eventsList != null) {
                commonUtils.updateEventWithMasterData(eventsList);
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(eventsList, id, Constants.SHIPMENT);
                shipmentDetails.setEventsList(updatedEvents);
                eventService.updateAtaAtdInShipment(updatedEvents, shipmentDetails, shipmentSettingsDetails);
            }
        }
        log.info("shipment afterSave eventDao.updateEntityFromOtherEntity.... ");

        // create Shipment event on the bases of auto create event flag
        if(isCreate && Boolean.TRUE.equals(shipmentSettingsDetails.getAutoEventCreate()))
            autoGenerateCreateEvent(shipmentDetails);
        log.info("shipment afterSave autoGenerateCreateEvent.... ");

        // Create events on basis of shipment status Confirmed/Created
        autoGenerateEvents(shipmentDetails);
        log.info("shipment afterSave generateEvents.... ");
    }

    @SuppressWarnings("java:S3655")
    private ConsolidationDetails makeConsoleNonDg(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, List<Long> removedConsolIds, ConsolidationDetails consolidationDetails) {
        if(!isCreate && removedConsolIds != null && !removedConsolIds.isEmpty()) {
            boolean makeConsoleNonDG = checkForDGShipmentAndAirDgFlag(oldEntity); // check if removed shipment was dg
            if(makeConsoleNonDG) {
                consolidationDetails = consolidationDetailsDao.findById(removedConsolIds.get(0)).get();
                if(!checkAttachDgAirShipments(consolidationDetails)) // check if any other attached shipment is dg
                    changeConsolidationDGValues(false, new AtomicBoolean(true), removedConsolIds.get(0), shipmentDetails, consolidationDetails);
            }
        }
        return consolidationDetails;
    }

    private List<UUID> getDeletedContGuids(List<Long> deleteContainerIds, List<UUID> deletedContGuids) {
        List<Containers> containers = containerDao.findByIdIn(deleteContainerIds);
        if (!CollectionUtils.isEmpty(containers)) {
            deletedContGuids = containers.stream().map(e -> e.getGuid()).toList();
        }
        return deletedContGuids;
    }

    private List<Packing> getPacksForSync(List<Long> deleteContainerIds, List<Packing> packsForSync) {
        List<Packing> packings = packingDao.findByContainerIdIn(deleteContainerIds);
        if (!CollectionUtils.isEmpty(packings)) {
            List<Packing> packingList = new ArrayList<>();
            for (Packing packing : packings) {
                packing.setContainerId(null);
                packingList.add(packing);
            }
            packingDao.saveAll(packingList);
            packsForSync = packingList;
        }
        return packsForSync;
    }

    private void storeMblAudit(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if (StringUtils.isNotBlank(shipmentDetails.getMasterBill())) {
            List<ConsolidationDetailsProjection> consolidations = consolidationDetailsDao.findMblNumberInDifferentTenant(shipmentDetails.getMasterBill());

            consolidations.forEach(consolidation -> {
                try {
                    if( ObjectUtils.isEmpty(oldEntity) || ObjectUtils.notEqual(oldEntity.getMasterBill(), shipmentDetails.getMasterBill())) {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                        .newData(MblDuplicatedLog.builder()
                                                .tenantId(consolidation.getTenantId())
                                                .consolidationNo(consolidation.getConsolidationNumber())
                                                .mblNumber(shipmentDetails.getMasterBill())
                                                .shipmentId(shipmentDetails.getShipmentId()).build())
                                        .prevData(null)
                                        .parent(ShipmentDetails.class.getSimpleName())
                                        .parentId(shipmentDetails.getId())
                                        .entityType(MblDuplicatedLog.class.getSimpleName())
                                        .operation(DBOperationType.LOG.name()).build()
                        );
                    }
                } catch (Exception e) {
                    log.error("Unable to store mbl check audit for shipment id: " + shipmentDetails.getId());
                }

            });
        }
    }

    private void processNetworkTransferEntity(Long tenantId, Long oldTenantId, ShipmentDetails shipmentDetails, String jobType) {
        try{
            networkTransferService.processNetworkTransferEntity(tenantId, oldTenantId, Constants.SHIPMENT, shipmentDetails,
                    null, jobType, null, false);
        } catch (Exception ex) {
            log.error("Exception during processing Network Transfer entity for shipment Id: {} with exception: {}", shipmentDetails.getShipmentId(), ex.getMessage());
        }
    }

    public void createOrUpdateNetworkTransferEntity(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        try{
            if(shipmentDetails.getDirection()!=null && Constants.DIRECTION_EXP.equals(shipmentDetails.getDirection()))
                processInterBranchEntityCase(shipmentDetails, oldEntity);
            if (isInterBranchConsole(shipmentDetails) || oldEntityHasInterBranchConsole(oldEntity))
                return;

            // Check if the shipment is eligible for network transfer
            if (isEligibleForNetworkTransfer(shipmentDetails)) {

                // Process the receiving branch for network transfer
                processNetworkTransferEntity(shipmentDetails.getReceivingBranch(),
                        oldEntity != null ? oldEntity.getReceivingBranch() : null, shipmentDetails,
                        reverseDirection(shipmentDetails.getDirection()));

                if (shipmentDetails.getTriangulationPartnerList() != null) {
                    processTriangulationPartnerList(shipmentDetails, oldEntity);
                } else if (shipmentDetails.getTriangulationPartner() != null) {
                    processEmptyTriangulationPartner(shipmentDetails, oldEntity);
                } else if(shipmentDetails.getTriangulationPartnerList() == null) {
                    processEmptyTriangulationPartnerList(shipmentDetails, oldEntity);
                }

            } else {
                // If not eligible for network transfer, handle deletion of old network transfer entities
                processNonEligibleNTE(oldEntity);

            }
        } catch (Exception ex) {
            log.error("Exception during creation or updation of Network Transfer entity for shipment Id: {} with exception: {}", shipmentDetails.getShipmentId(), ex.getMessage());
        }
    }

    private void processEmptyTriangulationPartner(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        processNetworkTransferEntity(shipmentDetails.getTriangulationPartner(),
                oldEntity != null ? oldEntity.getTriangulationPartner() : null, shipmentDetails,
                Constants.DIRECTION_CTS);
    }

    private void processNonEligibleNTE(ShipmentDetails oldEntity) {
        if(oldEntity !=null && oldEntity.getReceivingBranch() != null)
            networkTransferService.deleteValidNetworkTransferEntity(oldEntity.getReceivingBranch(),
                    oldEntity.getId(), Constants.SHIPMENT);

        // Delete network transfer entries for old triangulation partners
        if (oldEntity != null && ObjectUtils.isNotEmpty(oldEntity.getTriangulationPartnerList())) {
            for (TriangulationPartner triangularPartner : oldEntity.getTriangulationPartnerList()) {
                if (triangularPartner != null)
                    networkTransferService.deleteValidNetworkTransferEntity(triangularPartner.getTriangulationPartner(),
                        oldEntity.getId(), Constants.SHIPMENT);
            }
        } else if (oldEntity != null && oldEntity.getTriangulationPartnerList() == null
                && oldEntity.getTriangulationPartner() != null) {
            networkTransferService.deleteValidNetworkTransferEntity(oldEntity.getTriangulationPartner(),
                    oldEntity.getId(), Constants.SHIPMENT);
        }
    }

    private void processEmptyTriangulationPartnerList(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        List<Long> oldPartners = oldEntity != null ? commonUtils.getTriangulationPartnerList(oldEntity.getTriangulationPartnerList())
                : Collections.emptyList();
        Set<Long> oldTenantIds = new HashSet<>(oldPartners);
        oldTenantIds.forEach(oldTenantId -> processNetworkTransferEntity(null, oldTenantId, shipmentDetails, Constants.DIRECTION_CTS));
    }

    private void processTriangulationPartnerList(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        // Retrieve current and old triangulation partners
        List<Long> currentPartners = commonUtils.getTriangulationPartnerList(shipmentDetails.getTriangulationPartnerList());
        List<Long> oldPartners = oldEntity != null ? commonUtils.getTriangulationPartnerList(oldEntity.getTriangulationPartnerList())
                : Collections.emptyList();

        // Determine new tenant IDs by removing old partners from the current partners
        Set<Long> newTenantIds = new HashSet<>(currentPartners);
        newTenantIds.removeAll(oldPartners);

        // Determine old tenant IDs by removing current partners from the old partners
        Set<Long> oldTenantIds = new HashSet<>(oldPartners);
        oldTenantIds.removeAll(currentPartners);

        // Process new tenant IDs for network transfer
        newTenantIds.forEach(newTenantId -> processNetworkTransferEntity(newTenantId, null, shipmentDetails, Constants.DIRECTION_CTS));

        // Process old tenant IDs for removal from network transfer
        oldTenantIds.forEach(oldTenantId -> processNetworkTransferEntity(null, oldTenantId, shipmentDetails, Constants.DIRECTION_CTS));
    }

    public void triggerAutomaticTransfer(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, Boolean isDocAdded) {
        try{
            if (isInterBranchConsole(shipmentDetails) || oldEntityHasInterBranchConsole(oldEntity))
                return;
            Optional<QuartzJobInfo> optionalQuartzJobInfo = quartzJobInfoDao.findByJobFilters(
                    shipmentDetails.getTenantId(), shipmentDetails.getId(), SHIPMENT);

            QuartzJobInfo quartzJobInfo = optionalQuartzJobInfo.orElse(null);

            if(isEligibleForNetworkTransfer(shipmentDetails) && ObjectUtils.isNotEmpty(shipmentDetails.getReceivingBranch())){

                if (automaticTriggerNotValid(shipmentDetails, quartzJobInfo)) return;

                if (shouldCreateOrUpdateQuartzJob(quartzJobInfo, oldEntity, shipmentDetails, isDocAdded)) {
                    createOrUpdateQuartzJob(shipmentDetails, quartzJobInfo);
                }
            } else if (isEligibleForNetworkTransfer(shipmentDetails) && ObjectUtils.isEmpty(shipmentDetails.getReceivingBranch())
                && oldEntity != null && ObjectUtils.isNotEmpty(oldEntity.getReceivingBranch())) {
                commonErrorLogsDao.deleteShipmentErrorsLogs(shipmentDetails.getId());
            }
            if (hasHouseBillChange(shipmentDetails, oldEntity) || hasMasterBillChange(shipmentDetails, oldEntity)) {
                triggerConsoleTransfer(shipmentDetails);
            }
        } catch (Exception e) {
            log.error("Exception during creation or updation of Automatic transfer flow for shipment Id: {} with exception: {}", shipmentDetails.getShipmentId(), e.getMessage());
        }
    }

    private boolean automaticTriggerNotValid(ShipmentDetails shipmentDetails, QuartzJobInfo quartzJobInfo) {
        if (isInvalidNetworkTransfer(shipmentDetails)){
            commonErrorLogsDao.deleteShipmentErrorsLogs(shipmentDetails.getId());
            return true;
        }

        List<V1TenantSettingsResponse.FileTransferConfigurations> fileTransferConfigurations = quartzJobInfoService.getActiveFileTransferConfigurations(shipmentDetails.getTransportMode());
        if (ObjectUtils.isEmpty(fileTransferConfigurations)) {
            commonErrorLogsDao.deleteShipmentErrorsLogs(shipmentDetails.getId());
            return true;
        }

        if (isCarrierDetailsInvalid(shipmentDetails)) {
            handleInvalidCarrierDetails(quartzJobInfo);
            return true;
        }
        return false;
    }

    private boolean isInvalidNetworkTransfer(ShipmentDetails shipmentDetails) {
        Optional<NetworkTransfer> optionalNetworkTransfer = networkTransferDao.findByTenantAndEntity(
                Math.toIntExact(shipmentDetails.getReceivingBranch()), shipmentDetails.getId(), SHIPMENT);
        return optionalNetworkTransfer.isPresent() &&
                (optionalNetworkTransfer.get().getStatus() == NetworkTransferStatus.TRANSFERRED ||
                        optionalNetworkTransfer.get().getStatus() == NetworkTransferStatus.ACCEPTED);
    }

    private boolean isCarrierDetailsInvalid(ShipmentDetails shipmentDetails) {
        CarrierDetails carrierDetails = shipmentDetails.getCarrierDetails();
        return carrierDetails == null || (
                ObjectUtils.isEmpty(carrierDetails.getEta()) &&
                        ObjectUtils.isEmpty(carrierDetails.getEtd()) &&
                        ObjectUtils.isEmpty(carrierDetails.getAta()) &&
                        ObjectUtils.isEmpty(carrierDetails.getAtd()));
    }

    private void handleInvalidCarrierDetails(QuartzJobInfo quartzJobInfo) {
        if (quartzJobInfo != null && quartzJobInfo.getJobStatus() == JobState.QUEUED) {
            quartzJobInfoService.deleteJobById(quartzJobInfo.getId());
        }
    }

    private boolean shouldCreateOrUpdateQuartzJob(QuartzJobInfo quartzJobInfo, ShipmentDetails oldEntity, ShipmentDetails shipmentDetails, Boolean isDocAdded) {
        return (quartzJobInfo == null && oldEntity == null) ||
                shouldUpdateExistingJob(quartzJobInfo, oldEntity, shipmentDetails, isDocAdded);
    }

    private void triggerConsoleTransfer(ShipmentDetails shipmentDetails){
        if(ObjectUtils.isEmpty(shipmentDetails.getConsolidationList())){
            return;
        }

        for(ConsolidationDetails consolidationDetails: shipmentDetails.getConsolidationList()){
            Optional<QuartzJobInfo> optionalQuartzJobInfo = quartzJobInfoDao.findByJobFilters(
                    consolidationDetails.getTenantId(), consolidationDetails.getId(), CONSOLIDATION);

            QuartzJobInfo quartzJobInfo = optionalQuartzJobInfo.orElse(null);
            if(quartzJobInfo!=null && quartzJobInfo.getJobStatus()==JobState.ERROR){
                if(TRANSPORT_MODE_AIR.equals(consolidationDetails.getTransportMode()) &&
                        !Objects.equals(shipmentDetails.getJobType(), SHIPMENT_TYPE_DRT) &&
                        !Objects.equals(shipmentDetails.getJobType(), SHIPMENT_TYPE_STD)) {
                    consolidationService.triggerAutomaticTransfer(consolidationDetails, null, true);
                }
                if(TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode()) &&
                        !Objects.equals(shipmentDetails.getJobType(), SHIPMENT_TYPE_DRT)) {
                    consolidationService.triggerAutomaticTransfer(consolidationDetails, null, true);
                }
            }
        }
    }

    private boolean hasHouseBillChange(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        return (oldEntity == null && shipmentDetails.getHouseBill() != null)
                || (oldEntity != null && isValueChanged(shipmentDetails.getHouseBill(), oldEntity.getHouseBill()));
    }

    private boolean hasMasterBillChange(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        return (oldEntity == null && shipmentDetails.getMasterBill() != null)
                || (oldEntity != null && isValueChanged(shipmentDetails.getMasterBill(), oldEntity.getMasterBill()));
    }


    private boolean isValueChanged(Object newValue, Object oldValue) {
        return (oldValue != null && newValue==null) || (newValue != null && !newValue.equals(oldValue));
    }

    private void createOrUpdateQuartzJob(ShipmentDetails shipmentDetails, QuartzJobInfo existingJob) {
        CarrierDetails carrierDetails = shipmentDetails.getCarrierDetails();

        LocalDateTime jobTime = quartzJobInfoService.getQuartzJobTime(
                carrierDetails.getEta(), carrierDetails.getEtd(), carrierDetails.getAta(), carrierDetails.getAtd(),
                shipmentDetails.getTransportMode());

        if(jobTime == null)
            return;

        QuartzJobInfo quartzJobInfo = (existingJob != null) ? existingJob : createNewQuartzJob(shipmentDetails);
        quartzJobInfo.setJobStatus(JobState.QUEUED);
        quartzJobInfo.setStartTime(jobTime);
        quartzJobInfo.setErrorMessage(null);
        QuartzJobInfo newQuartzJobInfo = quartzJobInfoDao.save(quartzJobInfo);

        if(existingJob!=null && newQuartzJobInfo.getId()!=null && quartzJobInfoService.isJobWithNamePresent(newQuartzJobInfo.getId().toString())){
            quartzJobInfoService.updateSimpleJob(newQuartzJobInfo);
        }else{
            quartzJobInfoService.createSimpleJob(newQuartzJobInfo);
        }
        commonErrorLogsDao.deleteShipmentErrorsLogs(shipmentDetails.getId());
    }

    private QuartzJobInfo createNewQuartzJob(ShipmentDetails shipmentDetails) {
        return QuartzJobInfo.builder()
                .entityId(shipmentDetails.getId())
                .entityType(SHIPMENT)
                .tenantId(shipmentDetails.getTenantId())
                .jobType(JobType.SIMPLE_JOB)
                .build();
    }

    private boolean shouldUpdateExistingJob(QuartzJobInfo quartzJobInfo, ShipmentDetails oldEntity, ShipmentDetails shipmentDetails, Boolean isDocAdded) {
        return (isValidforAutomaticTransfer(quartzJobInfo, shipmentDetails, oldEntity, isDocAdded))
                || (isValidReceivingBranchChange(shipmentDetails, oldEntity));
    }

    private boolean isValidforAutomaticTransfer(QuartzJobInfo quartzJobInfo, ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, Boolean isDocAdded) {
        if (isValidDateChange(shipmentDetails, oldEntity))
            return true;

        if(quartzJobInfo==null ||( quartzJobInfo.getJobStatus() != JobState.ERROR))
            return false;

        if(Boolean.TRUE.equals(isDocAdded))
            return true;

        CarrierDetails newCarrierDetails = shipmentDetails.getCarrierDetails();

        // If oldCarrierDetails is null, check if newCarrierDetails has any populated fields.
        if (oldEntity == null || oldEntity.getCarrierDetails()==null) {
            return newCarrierDetails.getEta() != null
                    || newCarrierDetails.getEtd() != null
                    || newCarrierDetails.getAta() != null
                    || newCarrierDetails.getAtd() != null;
        }

        CarrierDetails oldCarrierDetails = oldEntity.getCarrierDetails();
        // Compare individual fields for changes.
        return isValueChanged(newCarrierDetails.getFlightNumber(), oldCarrierDetails.getFlightNumber());
    }

    private boolean isValidDateChange(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity){
        CarrierDetails newCarrierDetails = shipmentDetails.getCarrierDetails();
        if(oldEntity!=null && oldEntity.getCarrierDetails()!=null && newCarrierDetails!=null){
            CarrierDetails oldCarrierDetails = oldEntity.getCarrierDetails();
            return isValueChanged(newCarrierDetails.getEta(), oldCarrierDetails.getEta())
                    || isValueChanged(newCarrierDetails.getEtd(), oldCarrierDetails.getEtd())
                    || isValueChanged(newCarrierDetails.getAta(), oldCarrierDetails.getAta())
                    || isValueChanged(newCarrierDetails.getAtd(), oldCarrierDetails.getAtd());
        }
        return false;
    }

    private boolean isValidReceivingBranchChange(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if (oldEntity == null) {
            return false;
        }

        if (oldEntity.getReceivingBranch()==null) {
            return true;
        }

        boolean isBranchChanged = !Objects.equals(oldEntity.getReceivingBranch(), shipmentDetails.getReceivingBranch());
        if (!isBranchChanged) {
            return false;
        }

        Optional<NetworkTransfer> oldOptionalNetworkTransfer = networkTransferDao.findByTenantAndEntity(
                Math.toIntExact(oldEntity.getReceivingBranch()), oldEntity.getId(), SHIPMENT);

        return ((oldOptionalNetworkTransfer.isEmpty()) || oldOptionalNetworkTransfer
                .map(networkTransfer -> networkTransfer.getStatus() != NetworkTransferStatus.ACCEPTED)
                .orElse(false));
    }

    private boolean isEligibleForNetworkTransfer(ShipmentDetails details) {
        return TRANSPORT_MODE_AIR.equals(details.getTransportMode())
                && Constants.SHIPMENT_TYPE_DRT.equals(details.getJobType()) && Constants.DIRECTION_EXP.equals(details.getDirection());
    }

    private void processInterBranchEntityCase(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if (isInterBranchConsole(shipmentDetails)) {
            processReceivingBranchChanges(shipmentDetails, oldEntity);
        } else if ((shipmentDetails.getConsolidationList()==null || shipmentDetails.getConsolidationList().isEmpty()) && oldEntityHasInterBranchConsole(oldEntity)) {
            deleteOldConsolidationTransfers(oldEntity);
        }
    }

    private boolean isInterBranchConsole(ShipmentDetails shipmentDetails) {
        return shipmentDetails.getConsolidationList() != null
                && shipmentDetails.getConsolidationList().stream().anyMatch(consolidation -> Boolean.TRUE.equals(consolidation.getInterBranchConsole()));
    }

    private boolean oldEntityHasInterBranchConsole(ShipmentDetails oldEntity) {
        return oldEntity != null && oldEntity.getConsolidationList() != null &&
                oldEntity.getConsolidationList().stream().anyMatch(consolidation -> Boolean.TRUE.equals(consolidation.getInterBranchConsole()));
    }

    private boolean isConsoleAccepted(ConsolidationDetails console){
        List<NetworkTransfer> networkTransferList = networkTransferDao.getInterConsoleNTList(Collections.singletonList(console.getId()), CONSOLIDATION);
        if(networkTransferList!=null && !networkTransferList.isEmpty() ){
            for(NetworkTransfer networkTransfer: networkTransferList) {
                if(Objects.equals(networkTransfer.getJobType(), DIRECTION_CTS))
                    continue;
                if(networkTransfer.getStatus()==NetworkTransferStatus.ACCEPTED)
                    return true;
            }
        }
        return false;
    }

    private void processReceivingBranchChanges(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        ConsolidationDetails consolidationDetails = shipmentDetails.getConsolidationList().iterator().next();
        boolean isConsoleAcceptedCase = isConsoleAccepted(consolidationDetails);
        if(isConsoleAcceptedCase){
            return;
        }
        if(consolidationDetails.getReceivingBranch()!=null) {
            List<Long> shipmentIdsList = shipmentDetails.getConsolidationList().iterator().next().getShipmentsList().stream()
                    .map(ShipmentDetails::getId).toList();
            List<NetworkTransfer> nteList = networkTransferDao.getInterConsoleNTList(shipmentIdsList, SHIPMENT);
            Map<Long, NetworkTransfer> shipmentNetworkTransferMap = nteList!=null ? nteList.stream()
                    .collect(Collectors.toMap(NetworkTransfer::getEntityId, transfer -> transfer)) : null;
            NetworkTransfer existingNTE = shipmentNetworkTransferMap != null ? shipmentNetworkTransferMap.get(shipmentDetails.getId()) : null;
            if(existingNTE!=null && existingNTE.getStatus() == NetworkTransferStatus.ACCEPTED)
                return;
            if (shipmentDetails.getReceivingBranch() != null) {
                handleReceivingBranchUpdates(shipmentDetails, oldEntity, consolidationDetails, existingNTE, shipmentNetworkTransferMap);
            } else if (shouldDeleteOldTransfer(oldEntity, existingNTE)) {
                networkTransferService.deleteNetworkTransferEntity(existingNTE);
            }
        }else{
            List<NetworkTransfer> nteList = networkTransferDao.getInterConsoleNTList(Collections.singletonList(shipmentDetails.getId()), SHIPMENT);
            if(nteList!=null){
                nteList.forEach(nte -> networkTransferService.deleteNetworkTransferEntity(nte));
            }
        }
    }

    private void handleReceivingBranchUpdates(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity,
                                              ConsolidationDetails consolidationDetails, NetworkTransfer existingNTE,
                                              Map<Long, NetworkTransfer> shipmentNetworkTransferMap) {
        if (isBranchChanged(shipmentDetails, oldEntity)) {
            processBranchChangeOrUpdate(shipmentDetails, oldEntity, consolidationDetails, existingNTE, shipmentNetworkTransferMap);
        } else if (shouldUpdateConsole(shipmentDetails, consolidationDetails)) {
            processNetworkTransferEntity(shipmentDetails, existingNTE);
        } else {
            networkTransferService.deleteValidNetworkTransferEntity(shipmentDetails.getReceivingBranch(),
                    shipmentDetails.getId(), Constants.SHIPMENT);
        }
    }

    private boolean isBranchChanged(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        return oldEntity==null || oldEntity.getReceivingBranch()==null || (
                oldEntity.getReceivingBranch() != null &&
                        !Objects.equals(oldEntity.getReceivingBranch(), shipmentDetails.getReceivingBranch()));
    }

    private boolean shouldDeleteOldTransfer(ShipmentDetails oldEntity, NetworkTransfer existingNTE) {
        return oldEntity != null && oldEntity.getReceivingBranch() != null && existingNTE!=null &&
                Boolean.TRUE.equals(existingNTE.getIsInterBranchEntity());
    }

    private boolean shouldUpdateConsole(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails) {
        return !Objects.equals(shipmentDetails.getReceivingBranch(), consolidationDetails.getReceivingBranch());
    }

    private void processBranchChangeOrUpdate(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity,
                                             ConsolidationDetails consolidationDetails, NetworkTransfer existingNTE,
                                             Map<Long, NetworkTransfer> shipmentNetworkTransferMap) {
        if (!Objects.equals(shipmentDetails.getReceivingBranch(), consolidationDetails.getReceivingBranch())) {
            Long oldReceivingBranch = oldEntity!=null? oldEntity.getReceivingBranch(): null;
            networkTransferService.processNetworkTransferEntity(shipmentDetails.getReceivingBranch(), oldReceivingBranch,
                    Constants.SHIPMENT, shipmentDetails, null, reverseDirection(shipmentDetails.getDirection()), null, true);
        } else {
            if (existingNTE!=null) {
                networkTransferService.deleteNetworkTransferEntity(existingNTE);
                networkTransferService.bulkProcessInterConsoleNte(Collections.singletonList(shipmentDetails));
            } else {
                networkTransferService.bulkProcessInterConsoleNte(Collections.singletonList(shipmentDetails));
            }
        }
        // empty entity payload here
        updateNetworkTransfersForShipments(shipmentDetails.getId(), shipmentNetworkTransferMap);
        updateConsoleNetworkTransfer(consolidationDetails);
    }

    private void updateNetworkTransfersForShipments(Long currentShipmentId, Map<Long, NetworkTransfer> shipmentNetworkTransferMap) {
        if(shipmentNetworkTransferMap==null)
            return;
        shipmentNetworkTransferMap.values().stream()
                .filter(networkTransfer -> !Objects.equals(networkTransfer.getEntityId(), currentShipmentId))
                .forEach(this::resetNetworkTransferIfNeeded);
    }

    private void resetNetworkTransferIfNeeded(NetworkTransfer networkTransfer) {
        if (networkTransfer.getEntityPayload() != null && !networkTransfer.getEntityPayload().isEmpty()
                 && networkTransfer.getStatus() != NetworkTransferStatus.ACCEPTED) {
            if(networkTransfer.getStatus() != NetworkTransferStatus.REASSIGNED)
                networkTransfer.setStatus(NetworkTransferStatus.SCHEDULED);
            networkTransfer.setEntityPayload(null);
            networkTransferDao.save(networkTransfer);
        }
    }

    private void updateConsoleNetworkTransfer(ConsolidationDetails consolidationDetails) {
        Optional<NetworkTransfer> optionalConsoleNetworkTransfer = networkTransferDao
                .getInterConsoleNTList(Collections.singletonList(consolidationDetails.getId()), CONSOLIDATION).stream().findFirst();
        optionalConsoleNetworkTransfer.ifPresent(this::resetNetworkTransferIfNeeded);
    }

    private void processNetworkTransferEntity(ShipmentDetails shipmentDetails, NetworkTransfer existingNTE) {
        Long oldTenantId = (existingNTE != null && Objects.equals(existingNTE.getTenantId(), shipmentDetails.getReceivingBranch().intValue()))
                ? Long.valueOf(existingNTE.getTenantId()) : null;
        networkTransferService.processNetworkTransferEntity(shipmentDetails.getReceivingBranch(), oldTenantId, Constants.SHIPMENT,
                shipmentDetails, null, reverseDirection(shipmentDetails.getDirection()), null, true);
    }

    private void deleteOldConsolidationTransfers(ShipmentDetails oldEntity) {
        List<NetworkTransfer> networkTransferList = networkTransferDao.getInterConsoleNTList(Collections.singletonList(oldEntity.getId()), SHIPMENT);
        if(networkTransferList!=null) {
            for (NetworkTransfer networkTransfer : networkTransferList) {
                networkTransferService.deleteNetworkTransferEntity(networkTransfer);
            }
        }
    }

    private String reverseDirection(String direction) {
        String res = direction;
        if(Constants.DIRECTION_EXP.equalsIgnoreCase(direction)) {
            res = Constants.DIRECTION_IMP;
        }
        else if(Constants.DIRECTION_IMP.equalsIgnoreCase(direction)) {
            res = Constants.DIRECTION_EXP;
        }
        return res;
    }

    public List<Events> createOrUpdateEvents(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> updatedEvents, Boolean isNewShipment) {
        List<Events> newUpdatedEvents = (updatedEvents != null) ? new ArrayList<>(updatedEvents) : new ArrayList<>();

        if (Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEventsRevampEnabled())) {
            // we need db events now instead of the eventslist in shipment
            newUpdatedEvents = Optional.ofNullable(oldEntity).map(ShipmentDetails::getEventsList).orElse(new ArrayList<>());
        }

        // Update the direction if blank
        newUpdatedEvents.forEach(events -> events.setDirection(events.getDirection() == null ?
                shipmentDetails.getDirection() : events.getDirection()));

        createUpdateEvent(shipmentDetails, oldEntity, newUpdatedEvents, isNewShipment);

        // Update event fields for runner events generated
        // linking specific events to consol and populating other fields
        eventDao.updateFieldsForShipmentGeneratedEvents(newUpdatedEvents, shipmentDetails);

        return newUpdatedEvents;
    }

    private Map<String, List<Events>> groupCargoesRunnerEventsByCode(List<Events> events) {
        Map<String, List<Events>> eventMap = new HashMap<>();

        for (Events event : events) {
            String key = event.getEventCode();
            if(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER.equalsIgnoreCase(event.getSource())){
                if (eventMap.containsKey(key)) {
                    // Append the event to the existing list
                    eventMap.get(key).add(event);
                } else {
                    // Create a new list and add the event
                    List<Events> eventList = new ArrayList<>();
                    eventList.add(event);
                    eventMap.put(key, eventList);
                }
            }
        }
        return eventMap;
    }

    private void createUpdateEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment) {
        commonUtils.removeDuplicateTrackingEvents(events);
        Map<String, List<Events>> cargoesRunnerDbEvents = groupCargoesRunnerEventsByCode(events);
        oldEntity = Optional.ofNullable(oldEntity).orElse(new ShipmentDetails());
        oldEntity.setAdditionalDetails(Optional.ofNullable(oldEntity.getAdditionalDetails()).orElse(new AdditionalDetails()));
        oldEntity.setCarrierDetails(Optional.ofNullable(oldEntity.getCarrierDetails()).orElse(new CarrierDetails()));

        processLclOrFclOrAirEvents(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

        processLclOrAirEvents(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

        processLclEvents(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

        processEMCREvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

        processECCCEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

        processBLRSEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

        processFNMUEvent(shipmentDetails, oldEntity, isNewShipment, cargoesRunnerDbEvents);

        processCOODEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

    }

    private void processLclOrFclOrAirEvents(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (isLclOrFclOrAir(shipmentDetails)) {

            processBOCOEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

            processCADEEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

            processCACOEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

            processCUREEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

            processDOTPEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

            processPRDEEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

            processSEPUEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);
        }
    }

    private void processLclEvents(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (isLcl(shipmentDetails) || isFcl(shipmentDetails)) {

            processPUEDEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

            processTREDEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);
        }
    }

    private void processBOCOEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (isEventChanged(shipmentDetails.getBookingNumber(), oldEntity.getBookingNumber(), isNewShipment) && Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP)) {
            boolean shouldCreateBOCO = true;
            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.BOCO))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.BOCO);
                for (Events event : dbEvents) {
                    if (Objects.equals(shipmentDetails.getBookingNumber(), event.getContainerNumber())) {
                        event.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
                        eventDao.updateUserFieldsInEvent(event, true);
                        shouldCreateBOCO = false;
                    }
                }
            }
            createBOCOEvent(shipmentDetails, events, shouldCreateBOCO);
        }
    }

    private void createBOCOEvent(ShipmentDetails shipmentDetails, List<Events> events, boolean shouldCreateBOCO) {
        if(Boolean.TRUE.equals(shouldCreateBOCO)) {
            Events bocoEvent = initializeAutomatedEvents(shipmentDetails, EventConstants.BOCO, commonUtils.getUserZoneTime(LocalDateTime.now()), null);
            if (!CommonUtils.isStringNullOrEmpty(shipmentDetails.getBookingNumber()))
                bocoEvent.setContainerNumber(shipmentDetails.getBookingNumber());
            events.add(bocoEvent);
        }
    }

    private void processCADEEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventChanged(shipmentDetails.getAdditionalDetails().getCargoDeliveredDate(),
                        oldEntity.getAdditionalDetails().getCargoDeliveredDate(), isNewShipment)) {

            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.CADE))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.CADE);
                for (Events event : dbEvents) {
                    event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getCargoDeliveredDate()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            } else {
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.CADE,
                        commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getCargoDeliveredDate()), null));
            }
        }
    }

    private void processCACOEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventChanged(shipmentDetails.getAdditionalDetails().getPickupDate(),
                        oldEntity.getAdditionalDetails().getPickupDate(), isNewShipment)) {
            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.CACO))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.CACO);
                for (Events event : dbEvents) {
                    event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getPickupDate()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            } else {
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.CACO,
                        commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getPickupDate()), null));
            }
        }
    }

    private void processCUREEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventChanged(shipmentDetails.getAdditionalDetails().getCustomReleaseDate(),
                        oldEntity.getAdditionalDetails().getCustomReleaseDate(), isNewShipment)) {
            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.CURE))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.CURE);
                for (Events event : dbEvents) {
                    event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getCustomReleaseDate()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            } else {
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.CURE,
                        commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getCustomReleaseDate()), null));
            }
        }
    }

    private void processDOTPEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventBooleanChanged(shipmentDetails.getAdditionalDetails().getDocTurnedOverToCustomer(),
                        oldEntity.getAdditionalDetails().getDocTurnedOverToCustomer(), isNewShipment)) {

            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.DOTP))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.DOTP);
                for (Events event : dbEvents) {
                    event.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            } else {
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.DOTP,
                        commonUtils.getUserZoneTime(LocalDateTime.now()), null));
            }
        }
    }

    private void processPRDEEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventChanged(shipmentDetails.getAdditionalDetails().getProofOfDeliveryDate(),
                        oldEntity.getAdditionalDetails().getProofOfDeliveryDate(), isNewShipment)) {
            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.PRDE))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.PRDE);
                for (Events event : dbEvents) {
                    event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getProofOfDeliveryDate()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            } else {
            events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.PRDE,
                    commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getProofOfDeliveryDate()), null));
            }
        }
    }

    private void processSEPUEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventBooleanChanged(shipmentDetails.getAdditionalDetails().getPickupByConsigneeCompleted(),
                        oldEntity.getAdditionalDetails().getPickupByConsigneeCompleted(), isNewShipment)) {

            if(ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.SEPU))){
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.SEPU);
                for(Events event: dbEvents){
                    event.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            }else{
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.SEPU,
                        commonUtils.getUserZoneTime(LocalDateTime.now()), null));
            }
        }
    }

    private void processPUEDEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getCarrierDetails()) &&
                isEventChanged(shipmentDetails.getCarrierDetails().getEtd(),
                        oldEntity.getCarrierDetails().getEtd(), isNewShipment)) {

            if(ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.PUED))){
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.PUED);
                for(Events event: dbEvents){
                    event.setActual(shipmentDetails.getCarrierDetails().getEtd());
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            }else{
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.PUED,
                        shipmentDetails.getCarrierDetails().getEtd(), null));
            }
        }
    }

    private void processTREDEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getCarrierDetails()) &&
                isEventChanged(shipmentDetails.getCarrierDetails().getEta(),
                        oldEntity.getCarrierDetails().getEta(), isNewShipment)) {

            if(ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.TRED))){
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.TRED);
                for(Events event: dbEvents){
                    event.setActual(shipmentDetails.getCarrierDetails().getEta());
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            }else{
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.TRED,
                        shipmentDetails.getCarrierDetails().getEta(), null));
            }
        }
    }

    private void processLclOrAirEvents(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (isLclOrAir(shipmentDetails)) {
            if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                    isEventChanged(shipmentDetails.getAdditionalDetails().getWarehouseCargoArrivalDate(),
                            oldEntity.getAdditionalDetails().getWarehouseCargoArrivalDate(), isNewShipment)) {
                if(ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.CAFS))){
                    List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.CAFS);
                    for(Events event: dbEvents){
                        event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getWarehouseCargoArrivalDate()));
                        eventDao.updateUserFieldsInEvent(event, true);
                    }
                }else{
                    events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.CAFS,
                            commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getWarehouseCargoArrivalDate()), null));
                }
            }

            processCAAWEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);
        }
    }

    private void processCAAWEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (isEventChanged(shipmentDetails.getShipmentGateInDate(), oldEntity.getShipmentGateInDate(), isNewShipment) &&
                shipmentDetails.getDateType()!=null) {
            if(ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.CAAW))){
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.CAAW);
                processDbCaawEvent(shipmentDetails, dbEvents);
            }else{
                if(shipmentDetails.getDateType() == ACTUAL){
                    events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.CAAW,
                            commonUtils.getUserZoneTime(shipmentDetails.getShipmentGateInDate()), null));
                } else if (shipmentDetails.getDateType() == ESTIMATED) {
                    events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.CAAW, null,
                            commonUtils.getUserZoneTime(shipmentDetails.getShipmentGateInDate())));
                }
            }
        }
    }

    private void processDbCaawEvent(ShipmentDetails shipmentDetails, List<Events> dbEvents) {
        for(Events event: dbEvents){
            if(ACTUAL.equals(shipmentDetails.getDateType())) {
                event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getShipmentGateInDate()));
            }else if (ESTIMATED.equals(shipmentDetails.getDateType() )){
                event.setEstimated(commonUtils.getUserZoneTime(shipmentDetails.getShipmentGateInDate()));
            }
            eventDao.updateUserFieldsInEvent(event, true);
        }
    }

    private void processEMCREvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (isFcl(shipmentDetails) && ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventBooleanChanged(shipmentDetails.getAdditionalDetails().getEmptyContainerReturned(),
                        oldEntity.getAdditionalDetails().getEmptyContainerReturned(), isNewShipment)) {

            if(ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.EMCR))){
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.EMCR);
                for(Events event: dbEvents){
                    event.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            }else{
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.EMCR,
                        commonUtils.getUserZoneTime(LocalDateTime.now()), null));
            }
        }
    }

    private void processECCCEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventBooleanChanged(shipmentDetails.getAdditionalDetails().getIsExportCustomClearanceCompleted(),
                        oldEntity.getAdditionalDetails().getIsExportCustomClearanceCompleted(), isNewShipment)) {

            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.ECCC))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.ECCC);
                for (Events event : dbEvents) {
                    event.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            } else {
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.ECCC,
                        commonUtils.getUserZoneTime(LocalDateTime.now()), null));
            }
        }
    }

    private void processBLRSEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventChanged(shipmentDetails.getAdditionalDetails().getBlInstructionReceived(),
                        oldEntity.getAdditionalDetails().getBlInstructionReceived(), isNewShipment)) {

            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.BLRS))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.BLRS);
                for (Events event : dbEvents) {
                    event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getBlInstructionReceived()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            } else {
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.BLRS,
                        commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getBlInstructionReceived()), null));
            }
        }
    }

    private void processFNMUEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (isEventChanged(shipmentDetails.getMasterBill(), oldEntity.getMasterBill(), isNewShipment) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.FNMU))) {
            List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.FNMU);
            for (Events event : dbEvents) {
                event.setContainerNumber(shipmentDetails.getMasterBill());
                eventDao.updateUserFieldsInEvent(event, true);
            }
        }
    }

    private void processCOODEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventChanged(shipmentDetails.getAdditionalDetails().getCargoOutForDelivery(),
                        oldEntity.getAdditionalDetails().getCargoOutForDelivery(), isNewShipment)) {

            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.COOD))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.COOD);
                for (Events event : dbEvents) {
                    event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getCargoOutForDelivery()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            } else {
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.COOD,
                        commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getCargoOutForDelivery()), null));
            }
        }
    }

    private boolean isEventChanged(Object newValue, Object oldValue, Boolean isNewShipment) {
        return Boolean.TRUE.equals(isNewShipment) ? ObjectUtils.isNotEmpty(newValue) : !Objects.equals(newValue, oldValue);
    }

    private boolean isEventBooleanChanged(Boolean newValue, Boolean oldValue, Boolean isNewShipment) {
        return Boolean.TRUE.equals(newValue) && (Boolean.TRUE.equals(isNewShipment) || !Boolean.TRUE.equals(oldValue));
    }

    private boolean isLclOrFclOrAir(ShipmentDetails shipmentDetails) {
        return SHIPMENT_TYPE_LCL.equalsIgnoreCase(shipmentDetails.getShipmentType())
                || CARGO_TYPE_FCL.equalsIgnoreCase(shipmentDetails.getShipmentType())
                || TRANSPORT_MODE_AIR.equalsIgnoreCase(shipmentDetails.getTransportMode());
    }

    private boolean isLclOrAir(ShipmentDetails shipmentDetails) {
        return SHIPMENT_TYPE_LCL.equalsIgnoreCase(shipmentDetails.getShipmentType())
                || TRANSPORT_MODE_AIR.equalsIgnoreCase(shipmentDetails.getTransportMode());
    }

    private boolean isFcl(ShipmentDetails shipmentDetails) {
        return CARGO_TYPE_FCL.equalsIgnoreCase(shipmentDetails.getShipmentType());
    }

    private boolean isLcl(ShipmentDetails shipmentDetails) {
        return SHIPMENT_TYPE_LCL.equalsIgnoreCase(shipmentDetails.getShipmentType());
    }

    private boolean checkForAwbUpdate(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if(!Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) return false;
        if(!Objects.equals(shipmentDetails.getAdditionalDetails().getSci(), oldEntity.getAdditionalDetails().getSci())) return true;
        if(!Objects.equals(shipmentDetails.getSecurityStatus(), oldEntity.getSecurityStatus())) return true;
        return !Objects.equals(shipmentDetails.getAdditionalDetails().getEfreightStatus(), oldEntity.getAdditionalDetails().getEfreightStatus());
    }

    private void createShipmentRouteInConsole (ShipmentRequest shipmentRequest) throws RunnerException{
        Set<ConsolidationDetailsRequest> consoleRequest = shipmentRequest.getConsolidationList();
        List<Routings> createRoutes = new ArrayList<>();
        if(!Objects.isNull(shipmentRequest.getRoutingsList())) {
            if(shipmentRequest.getCreateMainLegRoute() != null && shipmentRequest.getCreateMainLegRoute()){
                List<RoutingsRequest> routeRequestList = shipmentRequest.getRoutingsList().stream().sorted(Comparator.comparingLong(RoutingsRequest::getLeg)).toList();
                var routeRequest = routeRequestList.stream().filter(x -> x.getMode().equals(shipmentRequest.getTransportMode())).findFirst();
                if(routeRequest.isPresent()) {
                    createRoutes.add(jsonHelper.convertValue(routeRequest.get(), Routings.class));
                    createRoutes = createConsoleRoutePayload(createRoutes);
                }
            } else {
                createRoutes = commonUtils.convertToEntityList(shipmentRequest.getRoutingsList(), Routings.class);
                createRoutes = createConsoleRoutePayload(createRoutes);
            }
        }
        if(consoleRequest != null && !consoleRequest.isEmpty() && !CommonUtils.listIsNullOrEmpty(createRoutes)) {
            for (var console : consoleRequest) {
                routingsDao.updateEntityFromConsole(createRoutes, console.getId());
            }
        }
    }

    private List<Routings> createConsoleRoutePayload(List<Routings> routes){
        List<Routings> responseList = new ArrayList<>();
        for (var route : routes){
            Routings routings = new Routings();
            routings.setLeg(1L);
            routings.setPol(route.getPol());
            routings.setPod(route.getPod());
            routings.setMode(route.getMode());
            routings.setEta(route.getEta());
            routings.setEtd(route.getEtd());
            routings.setTransitDays(route.getTransitDays());
            routings.setAta(route.getAta());
            routings.setAtd(route.getAtd());
            routings.setVesselName(route.getVesselName());
            routings.setVoyage(route.getVoyage());
            routings.setCarrier(route.getCarrier());
            routings.setFlightNumber(route.getFlightNumber());
            responseList.add(routings);
        }
        return responseList;
    }

    public ConsolidationDetails createConsolidation(ShipmentDetails shipmentDetails, List<Containers> containers) throws RunnerException {
        ShipmentSettingsDetails shipmentSettings = commonUtils.getShipmentSettingFromContext();
        if(Boolean.TRUE.equals(shipmentSettings.getShipConsolidationContainerEnabled())) {
            ConsolidationDetails consolidationDetails = new ConsolidationDetails();
            consolidationDetails.setConsolidationType(shipmentDetails.getJobType());
            consolidationDetails.setTransportMode(shipmentDetails.getTransportMode());
            validateCreateConsolidations(shipmentDetails, shipmentSettings);
            consolidationDetails.setCarrierDetails(jsonHelper.convertValue(shipmentDetails.getCarrierDetails(), CarrierDetails.class));
            consolidationDetails.getCarrierDetails().setId(null);
            consolidationDetails.getCarrierDetails().setGuid(null);
            if(shipmentSettings.getShipmentLite() != null && shipmentSettings.getShipmentLite() && shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetails.getDirection().equals(Constants.DIRECTION_EXP)) {
                consolidationDetails.setPayment(shipmentDetails.getPaymentTerms());
            }
            if(consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                consolidationDetails.getCarrierDetails().setOrigin(consolidationDetails.getCarrierDetails().getOriginPort());
                consolidationDetails.getCarrierDetails().setOriginLocCode(consolidationDetails.getCarrierDetails().getOriginPortLocCode());
                consolidationDetails.getCarrierDetails().setDestination(consolidationDetails.getCarrierDetails().getDestinationPort());
                consolidationDetails.getCarrierDetails().setDestinationLocCode(consolidationDetails.getCarrierDetails().getDestinationPortLocCode());
            }
            consolidationDetails.setShipmentType(shipmentDetails.getDirection());
            consolidationDetails.setContainerCategory(shipmentDetails.getShipmentType());
            consolidationDetails.setIsReceivingAgentFreeTextAddress(false);
            consolidationDetails.setIsSendingAgentFreeTextAddress(false);
            consolidationDetails.setIsInland(false);
            consolidationDetails.setCarrierBookingRef(shipmentDetails.getBookingNumber());
            consolidationDetails.setSourceTenantId(TenantContext.getCurrentTenant().longValue());
            if(StringUtility.isNotEmpty(shipmentDetails.getMasterBill())) {
                consolidationDetails.setBol(shipmentDetails.getMasterBill());
            }
            if(Objects.equals(TRANSPORT_MODE_SEA, shipmentDetails.getTransportMode()) || Objects.equals(TRANSPORT_MODE_AIR, shipmentDetails.getTransportMode()))
                consolidationDetails.setHazardous(shipmentDetails.getContainsHazardous());
            consolidationService.generateConsolidationNumber(consolidationDetails);
            addAgentDetailsForConsole(shipmentDetails, consolidationDetails);
            List<Routings> createRoutes = getRoutingsList(shipmentDetails, consolidationDetails);
            consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false, Boolean.TRUE.equals(shipmentDetails.getContainsHazardous()));
            if(!CommonUtils.listIsNullOrEmpty(createRoutes)) {
                routingsDao.saveEntityFromConsole(createRoutes, consolidationDetails.getId());
            }
            Long id = consolidationDetails.getId();
            setContainersInConsole(containers, id, consolidationDetails);
            createAutoEventCreate(shipmentSettings, consolidationDetails);
            consolidationService.pushShipmentDataToDependentService(consolidationDetails, true, null);
            return consolidationDetails;
        }
        return null;
    }

    private void setContainersInConsole(List<Containers> containers, Long id, ConsolidationDetails consolidationDetails) {
        if(containers != null && !containers.isEmpty()) {
            containers = containers.stream().map(e -> e.setConsolidationId(id)).toList();
            containers = containerDao.saveAll(containers);
        }
        consolidationDetails.setContainersList(containers);
    }

    private void createAutoEventCreate(ShipmentSettingsDetails shipmentSettings, ConsolidationDetails consolidationDetails) {
        if(shipmentSettings.getAutoEventCreate() != null && shipmentSettings.getAutoEventCreate()) {
            consolidationService.generateEvents(consolidationDetails);
        }
    }

    private void addAgentDetailsForConsole(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails) {
        if(shipmentDetails.getAdditionalDetails() != null) {
            consolidationDetails.setSendingAgent(commonUtils.removeIdFromParty(shipmentDetails.getAdditionalDetails().getExportBroker()));
            consolidationDetails.setReceivingAgent(commonUtils.removeIdFromParty(shipmentDetails.getAdditionalDetails().getImportBroker()));
        }
        if (Objects.equals(consolidationDetails.getShipmentType(), DIRECTION_EXP) && CommonUtils.checkAddressNotNull(consolidationDetails.getReceivingAgent())) {
            Long receivingBranchId = commonUtils.getReceivingBranch(consolidationDetails.getReceivingAgent().getOrgId(), consolidationDetails.getReceivingAgent().getAddressId());
            consolidationDetails.setReceivingBranch(receivingBranchId);
        }
        if(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getIsEntityTransferPrerequisiteEnabled())) {
            if(!commonUtils.checkIfPartyExists(consolidationDetails.getSendingAgent())) {
                consolidationDetails.setSendingAgentCountry(commonUtils.getCountryFromUnLocCode(consolidationDetails.getCarrierDetails().getOriginPortLocCode()));
            }
            if(!commonUtils.checkIfPartyExists(consolidationDetails.getReceivingAgent())) {
                consolidationDetails.setReceivingAgentCountry(commonUtils.getCountryFromUnLocCode(consolidationDetails.getCarrierDetails().getDestinationPortLocCode()));
            }
        }
    }

    private void validateCreateConsolidations(ShipmentDetails shipmentDetails, ShipmentSettingsDetails shipmentSettings) {
        if((shipmentSettings.getConsolidationLite() == null || !shipmentSettings.getConsolidationLite())
                && !Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_ROA)
                && (StringUtility.isEmpty(shipmentDetails.getCarrierDetails().getOriginPort()) || StringUtility.isEmpty(shipmentDetails.getCarrierDetails().getDestinationPort()))) {
            throw new ValidationException("Not able to create consolidation, before adding 'New Containers' , please provide ‘Origin’ and ‘Destination’ values.");
        }
        if(StringUtility.isNotEmpty(shipmentDetails.getCarrierDetails().getOriginPort()) && Objects.equals(shipmentDetails.getCarrierDetails().getOriginPort(), shipmentDetails.getCarrierDetails().getDestinationPort())) {
            throw new ValidationException("‘Origin’ and ‘Destination’ can't be same");
        }
    }

    private List<Routings> getRoutingsList(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails) {
        List<Routings> routings = new ArrayList<>();
        if(shipmentDetails.getRoutingsList() != null && !shipmentDetails.getRoutingsList().isEmpty())
            routings = shipmentDetails.getRoutingsList().stream().sorted(Comparator.comparingLong(Routings::getLeg)).toList();
        var routeRequest = routings.stream().filter(x -> x.getMode().equals(shipmentDetails.getTransportMode())).findFirst();
        List<Routings> createRoutes = new ArrayList<>();
        // Generate default Routes if Route Master is enabled
        if(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableRouteMaster())) {
            createRoutes.addAll(routingsDao.generateDefaultRouting(consolidationDetails.getCarrierDetails(), shipmentDetails.getTransportMode()));
            consolidationDetails.setRoutingsList(createRoutes);
        }
        else {
            if(routeRequest.isPresent()) {
                createRoutes.add(jsonHelper.convertValue(routeRequest.get(), Routings.class));
                createRoutes = createConsoleRoutePayload(createRoutes);
                consolidationDetails.setRoutingsList(createRoutes);
            }
        }
        return createRoutes;
    }

    @Override
    public void exportExcel(HttpServletResponse response, CommonRequestModel commonRequestModel) throws IOException, IllegalAccessException, ExecutionException, InterruptedException {
        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
        if (request == null) {
            log.error(ShipmentConstants.SHIPMENT_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException(ShipmentConstants.SHIPMENT_LIST_REQUEST_NULL_ERROR);
        }
        request.setIncludeTbls(Arrays.asList(Constants.ADDITIONAL_DETAILS, Constants.CLIENT, Constants.CONSIGNER, Constants.CONSIGNEE, Constants.CARRIER_DETAILS, Constants.PICKUP_DETAILS, Constants.DELIVERY_DETAILS));
        Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info(ShipmentConstants.SHIPMENT_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());
        Map<String, Integer> headerMap = new HashMap<>();
        for (int i = 0; i < ShipmentConstants.SHIPMENT_HEADERS.size(); i++) {
            headerMap.put(ShipmentConstants.SHIPMENT_HEADERS.get(i), i);
        }

        try(Workbook workbook = new XSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("ShipmentList");
            makeHeadersInSheet(sheet, workbook);

            //Filling the data
            List<IRunnerResponse> shipmentListResponseData = convertEntityListToDtoListForExport(shipmentDetailsPage.getContent());
            for (int i = 0; i < shipmentListResponseData.size(); i++) {
                processShipmentListResponseData(sheet, i, shipmentListResponseData, headerMap);
            }

            LocalDateTime currentTime = LocalDateTime.now();
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_HH_MM_SS_FORMAT);
            String timestamp = currentTime.format(formatter);
            String filenameWithTimestamp = "Shipments_" + timestamp + Constants.XLSX;
            String configuredLimitValue = applicationConfigService.getValue(EXPORT_EXCEL_LIMIT);
            Integer exportExcelLimit = StringUtility.isEmpty(configuredLimitValue) ? 1000 : Integer.parseInt(configuredLimitValue);
            if (shipmentListResponseData.size() > exportExcelLimit) {
                // Send the file via email
                commonUtils.sendExcelFileViaEmail(workbook, filenameWithTimestamp);
            } else {
                // Download it
                response.setContentType(Constants.CONTENT_TYPE_FOR_EXCEL);
                response.setHeader("Content-Disposition",
                    "attachment; filename=" + filenameWithTimestamp);

                try (OutputStream outputStream = new BufferedOutputStream(
                    response.getOutputStream(), 8192 * 10)) {
                    workbook.write(outputStream);
                } catch (IOException e) {
                    log.error("Time out " + e.getMessage());
                }
            }
        }

    }

    private void processShipmentListResponseData(Sheet sheet, int i, List<IRunnerResponse> shipmentListResponseData, Map<String, Integer> headerMap) throws IllegalAccessException {
        Row itemRow = sheet.createRow(i + 1);
        ShipmentListResponse shipment = (ShipmentListResponse) shipmentListResponseData.get(i);
        String origin = "";
        String destination = "";
        String destinationPort = "";
        String originPort = "";
        if(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getUnlocationData() != null){
            origin = shipment.getCarrierDetails().getUnlocationData().get(ShipmentConstants.ORIGIN);
            destination = shipment.getCarrierDetails().getUnlocationData().get(ShipmentConstants.DESTINATION);
            destinationPort = shipment.getCarrierDetails().getUnlocationData().get("destinationPort");
            originPort = shipment.getCarrierDetails().getUnlocationData().get("originPort");
        }
        if(shipment.getCarrierDetails() != null){
            origin = StringUtility.isEmpty(origin) ? shipment.getCarrierDetails().getOrigin() : origin;
            destination = StringUtility.isEmpty(destination) ? shipment.getCarrierDetails().getDestination() : destination;
            destinationPort = StringUtility.isEmpty(destinationPort) ? shipment.getCarrierDetails().getDestinationPort() : destinationPort;
            originPort = StringUtility.isEmpty(originPort) ? shipment.getCarrierDetails().getOriginPort() : originPort;
        }
        LocalTimeZoneHelper.transformTimeZone(shipment);
        processItemRows(headerMap, itemRow, shipment, origin, destination, originPort, destinationPort);
    }

    private void processItemRows(Map<String, Integer> headerMap, Row itemRow, ShipmentListResponse shipment, String origin, String destination, String originPort, String destinationPort) {
        itemRow.createCell(headerMap.get("Shipment Clone")).setCellValue("");
        itemRow.createCell(headerMap.get("Shipment Number")).setCellValue(shipment.getShipmentId());
        itemRow.createCell(headerMap.get("Order Number")).setCellValue(shipment.getOrderManagementNumber());
        itemRow.createCell(headerMap.get("Status")).setCellValue(String.valueOf(ShipmentStatus.values()[(shipment.getStatus())]));
        itemRow.createCell(headerMap.get("Transport Mode")).setCellValue(shipment.getTransportMode());
        itemRow.createCell(headerMap.get("Bill Status")).setCellValue(shipment.getBillStatus());
        itemRow.createCell(headerMap.get("MBL Number")).setCellValue(shipment.getMasterBill());
        itemRow.createCell(headerMap.get("Incoterm")).setCellValue(shipment.getIncoterms());
        itemRow.createCell(headerMap.get("Service Type")).setCellValue(shipment.getServiceType());
        itemRow.createCell(headerMap.get("Release Type")).setCellValue(Objects.isNull(shipment.getAdditionalDetails()) ? "" : shipment.getAdditionalDetails().getReleaseType());
        itemRow.createCell(headerMap.get("House Bill Type")).setCellValue(shipment.getAdditionalDetails() != null ? shipment.getAdditionalDetails().getHouseBillType() : "");
        itemRow.createCell(headerMap.get("Delivery Mode")).setCellValue(Objects.isNull(shipment.getDeliveryDetails()) ? "" : shipment.getDeliveryDetails().getDropMode());
        itemRow.createCell(headerMap.get("Consolidation Type")).setCellValue(String.valueOf(shipment.getJobType()));
        itemRow.createCell(headerMap.get("Activity Type")).setCellValue(Objects.isNull(shipment.getAdditionalDetails()) ? "" : shipment.getAdditionalDetails().getActivityType());
        itemRow.createCell(headerMap.get("Shipment Type")).setCellValue(shipment.getDirection());
        itemRow.createCell(headerMap.get("Carrier")).setCellValue(Objects.isNull(shipment.getCarrierDetails()) ? "" : shipment.getCarrierDetails().getShippingLine());
        addVesselNameItemRows(headerMap, itemRow, shipment);
        itemRow.createCell(headerMap.get("Flight Number")).setCellValue(Optional.ofNullable(shipment.getCarrierDetails()).map(c -> c.getFlightNumber()).orElse(""));
        itemRow.createCell(headerMap.get("Voyage/Flight No.")).setCellValue(Objects.isNull(shipment.getCarrierDetails()) ? "" : shipment.getCarrierDetails().getVoyage());
        addPaidAndIssuesPlaceName(headerMap, itemRow, shipment);
        itemRow.createCell(headerMap.get("Source1")).setCellValue(String.valueOf(shipment.getSource()));
        addDateOfIssueAndReceiptItemRows(headerMap, itemRow, shipment);
        itemRow.createCell(headerMap.get("Country of Origin")).setCellValue(Objects.isNull(shipment.getAdditionalDetails()) ? "" : shipment.getAdditionalDetails().getGoodsCO());
        addNotifyPartyNamesItemRow(headerMap, itemRow, shipment);
        itemRow.createCell(headerMap.get("Cargo Type")).setCellValue(shipment.getShipmentType());
        itemRow.createCell(headerMap.get("Origin")).setCellValue(origin);
        itemRow.createCell(headerMap.get("Destination")).setCellValue(destination);
        itemRow.createCell(headerMap.get("Domestic")).setCellValue(String.valueOf(shipment.getIsDomestic()));
        itemRow.createCell(headerMap.get("Route")).setCellValue(shipment.getRoute());
        addPartyNamesItemRow(headerMap, itemRow, shipment);
        itemRow.createCell(headerMap.get("HBL Number")).setCellValue(shipment.getHouseBill());
        itemRow.createCell(headerMap.get("BOE Number")).setCellValue(shipment.getAdditionalDetails() != null ? shipment.getAdditionalDetails().getBOENumber() : "");
        itemRow.createCell(headerMap.get("Screening Status")).setCellValue(shipment.getAdditionalDetails() != null ? shipment.getAdditionalDetails().getScreeningStatus() : "");
        addDateTimeDeliveryItemRows(headerMap, itemRow, shipment);
        itemRow.createCell(headerMap.get("Goods Description")).setCellValue(shipment.getGoodsDescription());
        itemRow.createCell(headerMap.get("Gross Weight")).setCellValue(String.valueOf(shipment.getWeight()));
        itemRow.createCell(headerMap.get("Gross Weight Unit")).setCellValue(shipment.getWeightUnit());
        itemRow.createCell(headerMap.get("Volume")).setCellValue(String.valueOf(shipment.getVolume()));
        itemRow.createCell(headerMap.get("Volume Unit")).setCellValue(shipment.getVolumeUnit());
        itemRow.createCell(headerMap.get("Chargeable Weight")).setCellValue(String.valueOf(shipment.getChargable()));
        itemRow.createCell(headerMap.get("Volumetric Weight")).setCellValue(String.valueOf(shipment.getVolumetricWeight()));
        itemRow.createCell(headerMap.get("No. Of Packages")).setCellValue(String.valueOf(shipment.getNoOfPacks()));
        itemRow.createCell(headerMap.get("Package Type")).setCellValue(String.valueOf((shipment.getPacksUnit())));
        itemRow.createCell(headerMap.get("No. Of Inner Packages")).setCellValue(String.valueOf(shipment.getInnerPacks()));
        itemRow.createCell(headerMap.get("IU")).setCellValue("");
        itemRow.createCell(headerMap.get("Customer Booking Number")).setCellValue(String.valueOf(shipment.getBookingNumber()));
        addPickUpAndDelivertTransporterItemRow(headerMap, itemRow, shipment);
        itemRow.createCell(headerMap.get("Job Status")).setCellValue(String.valueOf(shipment.getJobStatus()));
        itemRow.createCell(headerMap.get("Assigned To")).setCellValue(String.valueOf(shipment.getAssignedTo()));
        itemRow.createCell(headerMap.get("Created By")).setCellValue(String.valueOf(shipment.getCreatedBy()));
        itemRow.createCell(headerMap.get("Created Source")).setCellValue(String.valueOf(shipment.getSource()));
        itemRow.createCell(headerMap.get("Updated Date")).setCellValue(String.valueOf(shipment.getUpdatedAt()));
        itemRow.createCell(headerMap.get("20RE")).setCellValue(String.valueOf(shipment.getContainer20RECount()));
        itemRow.createCell(headerMap.get("20GP")).setCellValue(String.valueOf(shipment.getContainer20GPCount()));
        itemRow.createCell(headerMap.get("40RE")).setCellValue(String.valueOf(shipment.getContainer40RECount()));
        itemRow.createCell(headerMap.get("40GP")).setCellValue(String.valueOf(shipment.getContainer40GPCount()));
        itemRow.createCell(headerMap.get("Container Number")).setCellValue(shipment.getContainerNumbers() != null && !shipment.getContainerNumbers().isEmpty() ? shipment.getContainerNumbers().stream().findFirst().orElse("") : "");
        itemRow.createCell(headerMap.get("Created Date")).setCellValue(String.valueOf(shipment.getCreatedAt()));
        addRevenueItemRows(headerMap, itemRow, shipment);
        itemRow.createCell(headerMap.get("20s Count")).setCellValue(String.valueOf(shipment.getContainer20Count()));
        itemRow.createCell(headerMap.get("40s Count")).setCellValue(String.valueOf(shipment.getContainer40Count()));
        itemRow.createCell(headerMap.get("TEU Count")).setCellValue(shipment.getTeuCount() != null ? shipment.getTeuCount().toString() : null);
        itemRow.createCell(headerMap.get("CreatedBy")).setCellValue(shipment.getCreatedBy());
        itemRow.createCell(headerMap.get("POL")).setCellValue(originPort);
        itemRow.createCell(headerMap.get("POD")).setCellValue(destinationPort);
        itemRow.createCell(headerMap.get("Waybill Number")).setCellValue(String.valueOf(shipment.getWayBillNumber()));
        itemRow.createCell(headerMap.get("Additional Terms")).setCellValue(String.valueOf(shipment.getAdditionalTerms()));
        itemRow.createCell(headerMap.get("Reference Number")).setCellValue(String.valueOf(shipment.getBookingReference()));
        addOriginDestinationPortCodeRows(headerMap, itemRow, shipment);
    }

    private void addPickUpAndDelivertTransporterItemRow(Map<String, Integer> headerMap, Row itemRow, ShipmentListResponse shipment) {
        itemRow.createCell(headerMap.get("Pickup Transporter")).setCellValue(shipment.getPickupDetails() != null && shipment.getPickupDetails().getTransporterDetail() != null && shipment.getPickupDetails().getTransporterDetail().getOrgData() != null ? String.valueOf(shipment.getPickupDetails().getTransporterDetail().getOrgData().get(ShipmentConstants.FULL_NAME)) : "");
        itemRow.createCell(headerMap.get("Delivery Transporter")).setCellValue(shipment.getDeliveryDetails() != null && shipment.getDeliveryDetails().getTransporterDetail() != null && shipment.getDeliveryDetails().getTransporterDetail().getOrgData() != null ? String.valueOf(shipment.getDeliveryDetails().getTransporterDetail().getOrgData().get(ShipmentConstants.FULL_NAME)) : "");
    }

    private void addDateOfIssueAndReceiptItemRows(Map<String, Integer> headerMap, Row itemRow, ShipmentListResponse shipment) {
        itemRow.createCell(headerMap.get("Date of Issue")).setCellValue(Objects.isNull(shipment.getAdditionalDetails()) || Objects.isNull(shipment.getAdditionalDetails().getDateOfIssue()) ? "" : shipment.getAdditionalDetails().getDateOfIssue().toString());
        itemRow.createCell(headerMap.get("Date of Receipt")).setCellValue(Objects.isNull(shipment.getAdditionalDetails()) || Objects.isNull(shipment.getAdditionalDetails().getDateOfReceipt()) ? "" : shipment.getAdditionalDetails().getDateOfReceipt().toString());
    }

    private void addPaidAndIssuesPlaceName(Map<String, Integer> headerMap, Row itemRow, ShipmentListResponse shipment) {
        itemRow.createCell(headerMap.get("Paid Place Name")).setCellValue(shipment.getAdditionalDetails() != null && shipment.getAdditionalDetails().getUnlocationData() != null ? String.valueOf(shipment.getAdditionalDetails().getUnlocationData().get(ShipmentConstants.PAID_PLACE)) : "");
        itemRow.createCell(headerMap.get("Issued Place Name")).setCellValue(shipment.getAdditionalDetails() != null && shipment.getAdditionalDetails().getUnlocationData() != null ? String.valueOf(shipment.getAdditionalDetails().getUnlocationData().get(ShipmentConstants.PLACE_OF_ISSUE)) : "");
    }

    private void addNotifyPartyNamesItemRow(Map<String, Integer> headerMap, Row itemRow, ShipmentListResponse shipment) {
        itemRow.createCell(headerMap.get("Notify Party Name")).setCellValue(shipment.getAdditionalDetails() != null && shipment.getAdditionalDetails().getNotifyParty() != null && shipment.getAdditionalDetails().getNotifyParty().getOrgData() != null ?
                String.valueOf(shipment.getAdditionalDetails().getNotifyParty().getOrgData().get(ShipmentConstants.FULL_NAME)) : "");
    }

    private void addVesselNameItemRows(Map<String, Integer> headerMap, Row itemRow, ShipmentListResponse shipment) {
        itemRow.createCell(headerMap.get("Vessel Name/Flight")).setCellValue(shipment.getCarrierDetails() != null &&  shipment.getCarrierDetails().getVesselsMasterData()!= null? shipment.getCarrierDetails().getVesselsMasterData().get("vessel") : "");
    }

    private void addPartyNamesItemRow(Map<String, Integer> headerMap, Row itemRow, ShipmentListResponse shipment) {
        itemRow.createCell(headerMap.get("Client Name")).setCellValue(shipment.getClient() != null && shipment.getClient().getOrgData() != null ? shipment.getClient().getOrgData().getOrDefault(PartiesConstants.FULLNAME, "").toString() : "");
        itemRow.createCell(headerMap.get("Consignor Name")).setCellValue(shipment.getConsigner() != null && shipment.getConsigner().getOrgData() != null ? shipment.getConsigner().getOrgData().getOrDefault(PartiesConstants.FULLNAME, "").toString() : "");
        itemRow.createCell(headerMap.get("Consignee Name")).setCellValue(shipment.getConsignee() != null && shipment.getConsignee().getOrgData() != null ? shipment.getConsignee().getOrgData().getOrDefault(PartiesConstants.FULLNAME, "").toString() : "");
    }

    private void addDateTimeDeliveryItemRows(Map<String, Integer> headerMap, Row itemRow, ShipmentListResponse shipment) {
        itemRow.createCell(headerMap.get("BOE Date")).setCellValue(shipment.getAdditionalDetails() != null && shipment.getAdditionalDetails().getBOEDate() != null ? shipment.getAdditionalDetails().getBOEDate().toString() : "");
        itemRow.createCell(headerMap.get("ETA")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getEta() != null ? shipment.getCarrierDetails().getEta().toString() : "");
        itemRow.createCell(headerMap.get("ETD")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getEtd() != null ? shipment.getCarrierDetails().getEtd().toString() : "");
        itemRow.createCell(headerMap.get("ATD")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getAtd() != null ? shipment.getCarrierDetails().getAtd().toString() : "");
        itemRow.createCell(headerMap.get("ATA")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getAta() != null ? shipment.getCarrierDetails().getAta().toString() : "");
        itemRow.createCell(headerMap.get("Estimated Delivery")).setCellValue(shipment.getDeliveryDetails() != null && shipment.getDeliveryDetails().getEstimatedPickupOrDelivery() != null ? shipment.getDeliveryDetails().getEstimatedPickupOrDelivery().toString() : "");
        itemRow.createCell(headerMap.get("Actual Delivery")).setCellValue(shipment.getDeliveryDetails() != null && shipment.getDeliveryDetails().getActualPickupOrDelivery() != null ? shipment.getDeliveryDetails().getActualPickupOrDelivery().toString() : "");
    }

    private void addOriginDestinationPortCodeRows(Map<String, Integer> headerMap, Row itemRow, ShipmentListResponse shipment) {
        itemRow.createCell(headerMap.get("POL Code")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getUnlocationData() != null ? String.valueOf(shipment.getCarrierDetails().getUnlocationData().get("originPort_code")) : "");
        itemRow.createCell(headerMap.get("POD Code")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getUnlocationData() != null ? String.valueOf(shipment.getCarrierDetails().getUnlocationData().get("destinationPort_code")) : "");
        itemRow.createCell(headerMap.get("Origin Code")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getUnlocationData() != null ? String.valueOf(shipment.getCarrierDetails().getUnlocationData().get("origin_code")) : "");
        itemRow.createCell(headerMap.get("Destination Code")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getUnlocationData() != null ? String.valueOf(shipment.getCarrierDetails().getUnlocationData().get("destination_code")) : "");
    }

    private void addRevenueItemRows(Map<String, Integer> headerMap, Row itemRow, ShipmentListResponse shipment) {
        itemRow.createCell(headerMap.get("Estimated Cost")).setCellValue(shipment.getTotalEstimatedCost() != null ? shipment.getTotalEstimatedCost().toString() : "");
        itemRow.createCell(headerMap.get("Estimated Revenue")).setCellValue(shipment.getTotalEstimatedRevenue() != null ? shipment.getTotalEstimatedRevenue().toString() : "");
        itemRow.createCell(headerMap.get("Estimated Profit")).setCellValue(shipment.getTotalEstimatedProfit() != null ? shipment.getTotalEstimatedProfit().toString() : "");
        itemRow.createCell(headerMap.get("Estimated Profit %")).setCellValue(shipment.getTotalEstimatedProfitPercent() != null ? shipment.getTotalEstimatedProfitPercent().toString() : "");
        itemRow.createCell(headerMap.get("Captured Cost")).setCellValue(shipment.getTotalCost() != null ? shipment.getTotalCost().toString() : "");
        itemRow.createCell(headerMap.get("Captured Revenue")).setCellValue(shipment.getTotalRevenue() != null ? shipment.getTotalRevenue().toString() : "");
        itemRow.createCell(headerMap.get("Captured Profit")).setCellValue(shipment.getTotalProfit() != null ? shipment.getTotalProfit().toString() : "");
        itemRow.createCell(headerMap.get("Captured Profit %")).setCellValue(shipment.getTotalProfitPercent() != null ? shipment.getTotalProfitPercent().toString() : "");
        itemRow.createCell(headerMap.get("Invoiced Payable Cost")).setCellValue(shipment.getTotalPostedProfit() != null ? shipment.getTotalPostedCost().toString() : "");
        itemRow.createCell(headerMap.get("Invoiced Receivable Revenue")).setCellValue(shipment.getTotalPostedRevenue() != null ? shipment.getTotalPostedRevenue().toString() : "");
        itemRow.createCell(headerMap.get("Invoiced Profit")).setCellValue(shipment.getTotalPostedProfit() != null ? shipment.getTotalPostedProfit().toString() : "");
        itemRow.createCell(headerMap.get("Invoiced Profit %")).setCellValue(shipment.getTotalPostedProfitPercent() != null ? shipment.getTotalPostedProfitPercent().toString() : "");
    }

    private void makeHeadersInSheet(Sheet sheet, Workbook workbook) {
        Row headerRow = sheet.createRow(0);
        List<String> shipmentHeader = ShipmentConstants.SHIPMENT_HEADERS;

        CellStyle boldStyle = workbook.createCellStyle();
        Font boldFont = workbook.createFont();
        boldFont.setBold(true);
        boldStyle.setFont(boldFont);

        for (int i = 0; i < shipmentHeader.size(); i++) {
            Cell cell = headerRow.createCell(i);
            cell.setCellValue(shipmentHeader.get(i));
            cell.setCellStyle(boldStyle);
        }
    }


    public ResponseEntity<IRunnerResponse> fullShipmentsList(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(ShipmentConstants.SHIPMENT_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException(ShipmentConstants.SHIPMENT_LIST_REQUEST_NULL_ERROR);
            }
            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = this.findAllWithOutIncludeColumn(tuple.getLeft(), tuple.getRight());
            log.info(ShipmentConstants.SHIPMENT_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());
            if(request.getIncludeColumns()==null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildListSuccessResponse(
                        convertEntityListToFullShipmentList(shipmentDetailsPage.getContent()),
                        shipmentDetailsPage.getTotalPages(),
                        shipmentDetailsPage.getTotalElements());
            else {
                List<IRunnerResponse>filteredList=new ArrayList<>();
                for( var curr: convertEntityListToFullShipmentList(shipmentDetailsPage.getContent())){
                    RunnerPartialListResponse res=new RunnerPartialListResponse();
                    res.setData(partialFetchUtils.fetchPartialListData(curr,request.getIncludeColumns()));
                    filteredList.add( res);

                }
                return ResponseHelper.buildListSuccessResponse(
                        filteredList,
                        shipmentDetailsPage.getTotalPages(),
                        shipmentDetailsPage.getTotalElements());
            }
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> fullShipmentsExternalList(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(ShipmentConstants.SHIPMENT_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException(ShipmentConstants.SHIPMENT_LIST_REQUEST_NULL_ERROR);
            }
            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info(ShipmentConstants.SHIPMENT_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());
            if(request.getIncludeColumns()==null || request.getIncludeColumns().isEmpty()) {
                throw new ValidationException("Include Columns field is mandatory");
            }
            List<IRunnerResponse>filteredList=new ArrayList<>();

            for( var curr:shipmentDetailsPage.getContent()){
                ShipmentDetailsResponse shipmentDetailsResponse = (ShipmentDetailsResponse) commonUtils.setIncludedFieldsToResponse(curr, request.getIncludeColumns().stream().collect(Collectors.toSet()), new ShipmentDetailsResponse());
                if (request.getIncludeColumns().contains(ShipmentConstants.CONSOLIDATION_NUMBER)) {
                    Set<ConsolidationDetails> consolidationDetailsSet = curr.getConsolidationList();
                    if(!CollectionUtils.isEmpty(consolidationDetailsSet)) {
                        ConsolidationDetails consolidationDetails = consolidationDetailsSet.stream().findFirst().get();
                        shipmentDetailsResponse.setConsolidationNumber(consolidationDetails.getConsolidationNumber());
                    }
                }
                // from dps we will receive one guid at a time
                if (request.getIncludeColumns().contains(ShipmentConstants.IMPLICATIONS_LIST_COLUMN)) {
                    shipmentDetailsResponse.setImplicationList(dpsEventService.getImplicationsForShipment(shipmentDetailsResponse.getGuid().toString()));
                }
                filteredList.add(shipmentDetailsResponse);
            }
            return ResponseHelper.buildListSuccessResponse(
                    filteredList,
                    shipmentDetailsPage.getTotalPages(),
                    shipmentDetailsPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }


    private ListCommonRequest setCriteriaToFetchSimilarShipment(ListCommonRequest request, ShipmentDetails shipmentDetails) {
        if (request.getFilterCriteria() != null && request.getFilterCriteria().isEmpty()) {
            request.setFilterCriteria(Arrays.asList(FilterCriteria.builder().innerFilter(new ArrayList<>()).build()));
        }
        ListCommonRequest defaultRequest = CommonUtils.andCriteria(Constants.TRANSPORT_MODE,
                shipmentDetails.getTransportMode(), "=", request);

        Map<String, Object> shipmentFieldNameValueMap = new HashMap<>();
        shipmentFieldNameValueMap.put(Constants.DIRECTION, shipmentDetails.getDirection());
        shipmentFieldNameValueMap.put(Constants.SHIPMENT_TYPE, shipmentDetails.getShipmentType());
        shipmentFieldNameValueMap.put(Constants.JOB_TYPE, shipmentDetails.getJobType());
        shipmentFieldNameValueMap.put(Constants.INCOTERMS, shipmentDetails.getIncoterms());

        if(!Objects.isNull(shipmentDetails.getCarrierDetails())){
            shipmentFieldNameValueMap.put(Constants.ORIGIN_PORT, shipmentDetails.getCarrierDetails().getOriginPort());
            shipmentFieldNameValueMap.put(Constants.DESTINATION_PORT, shipmentDetails.getCarrierDetails().getDestinationPort());
        }

        if (!Objects.isNull(shipmentDetails.getClient())) {
            shipmentFieldNameValueMap.put(Constants.CLIENT_ORG_CODE, shipmentDetails.getClient().getOrgCode());
            shipmentFieldNameValueMap.put(Constants.CLIENT_ADDRESS_CODE, shipmentDetails.getClient().getAddressCode());
        }
        if (!Objects.isNull(shipmentDetails.getConsigner())) {
            shipmentFieldNameValueMap.put(Constants.CONSIGNER_ORG_CODE, shipmentDetails.getConsigner().getOrgCode());
            shipmentFieldNameValueMap.put(Constants.CONSIGNER_ADDRESS_CODE, shipmentDetails.getConsigner().getAddressCode());
        }
        if (!Objects.isNull(shipmentDetails.getConsignee())) {
            shipmentFieldNameValueMap.put(Constants.CONSIGNEE_ORG_CODE, shipmentDetails.getConsignee().getOrgCode());
            shipmentFieldNameValueMap.put(Constants.CONSIGNEE_ADDRESS_CODE, shipmentDetails.getConsignee().getAddressCode());
        }
        addCriteriaToFilter(request, shipmentFieldNameValueMap);
        addLikeCriteriaToFilter(request, request.getEntityId());
        addCriteriaToExclude(defaultRequest, shipmentDetails);

        return defaultRequest;
    }

    private void addCriteriaToFilter(ListCommonRequest request, Map<String, Object> shipmentFieldNameValueMap) {
        for (Map.Entry<String, Object> entry : shipmentFieldNameValueMap.entrySet()) {
            if (!Objects.isNull(entry.getValue())) {
                CommonUtils.andCriteria(entry.getKey(), entry.getValue(), "=", request);
            }
        }
    }

    private void addLikeCriteriaToFilter(ListCommonRequest request, String shipmentId){
        if(shipmentId != null) {
            CommonUtils.andCriteria(Constants.SHIPMENT_ID, shipmentId, "LIKE", request);
        }
    }

    private void addCriteriaToExclude(ListCommonRequest request, ShipmentDetails shipmentDetails) {
        CommonUtils.andCriteria(Constants.STATUS, ShipmentStatus.NonMovement.getValue(), "!=", request);
        CommonUtils.andCriteria(Constants.STATUS, ShipmentStatus.Cancelled.getValue(), "!=", request);
        CommonUtils.andCriteria(Constants.GUID, shipmentDetails.getGuid(), "!=", request);
    }

    @Override
    public ResponseEntity<IRunnerResponse> fetchBillChargesShipmentList(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            if (commonRequestModel.getGuid() == null) {
                log.error(ShipmentConstants.REQUIRED_PARAMETER_MISSING_ERROR, ShipmentConstants.SHIPMENT_GUID, LoggerHelper.getRequestIdFromMDC());
            }

            Optional<ShipmentDetails> optionalShipmentDetails = shipmentDao.findByGuid(UUID.fromString(commonRequestModel.getGuid()));
            if (!optionalShipmentDetails.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_DETAILS_FOR_GUID_MISSING_ERROR, commonRequestModel.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            ShipmentDetails shipmentDetails = optionalShipmentDetails.get();
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            request.setIncludeTbls(Arrays.asList(Constants.PICKUP_DETAILS, Constants.DELIVERY_DETAILS, Constants.ADDITIONAL_DETAILS, Constants.CLIENT, Constants.CONSIGNER, Constants.CONSIGNEE, Constants.CARRIER_DETAILS));
            ListCommonRequest listRequest = setCriteriaToFetchSimilarShipment(request, shipmentDetails);
            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(listRequest, ShipmentDetails.class, tableNames);
            Specification<ShipmentDetails> spec = tuple.getLeft();
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(spec , tuple.getRight());
            log.info(ShipmentConstants.SHIPMENT_LIST_SIMILAR_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());
            if(request.getIncludeColumns()==null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildListSuccessResponse(
                        convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                        shipmentDetailsPage.getTotalPages(), shipmentDetailsPage.getTotalElements());
            else {
                List<IRunnerResponse>filteredList=new ArrayList<>();
                for( var curr: convertEntityListToDtoList(shipmentDetailsPage.getContent())){
                    RunnerPartialListResponse res=new RunnerPartialListResponse();
                    res.setData(partialFetchUtils.fetchPartialListData(curr, request.getIncludeColumns()));
                    filteredList.add( res);
                }
                return ResponseHelper.buildListSuccessResponse(filteredList, shipmentDetailsPage.getTotalPages(),
                        shipmentDetailsPage.getTotalElements());
            }
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> assignShipmentContainers(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ShipmentContainerAssignRequest request = (ShipmentContainerAssignRequest) commonRequestModel.getData();
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            ShipmentDetails shipmentDetails = shipmentDao.findById(request.getShipmentId()).get();
            Set<Containers> oldContainers = shipmentDetails.getContainersList();
            ListCommonRequest listCommonRequest = constructListCommonRequest("id", request.getContainerIds(), "IN");
            Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
            Page<Containers> containers = containerDao.findAll(pair.getLeft(), pair.getRight());
            Map<Long, Containers> containersMap = new HashMap<>();
            if(Boolean.TRUE.equals(shipmentSettingsDetails.getMultipleShipmentEnabled()) && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA))) {
                updateContainerMap(shipmentDetails, containers, containersMap);
            }
            else {
                containersMap = containers.getContent().stream().collect(Collectors.toMap(e -> e.getId(), c -> c));
            }
            shipmentsContainersMappingDao.assignContainers(request.getShipmentId(), request.getContainerIds(), shipmentDetails.getGuid().toString());
            makeShipmentsDG(containersMap, shipmentDetails);
            dependentServiceHelper.pushShipmentDataToDependentService(shipmentDetails, false, false, oldContainers);
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private void updateContainerMap(ShipmentDetails shipmentDetails, Page<Containers> containers, Map<Long, Containers> containersMap) {
        boolean isFCL = shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL) && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA));
        if(containers != null) {
            List<Containers> containersList = containers.getContent();
            if(!containers.getContent().isEmpty()) {
                for (Containers container : containersList) {
                    validateContainers(shipmentDetails, container);
                    if(isFCL) {
                        container.setAchievedWeight(container.getAllocatedWeight());
                        container.setAchievedVolume(container.getAllocatedVolume());
                        container.setAchievedWeightUnit(container.getAllocatedWeightUnit());
                        container.setAchievedVolumeUnit(container.getAllocatedVolumeUnit());
                        container.setWeightUtilization("100");
                        container.setVolumeUtilization("100");
                    }
                    containersMap.put(container.getId(), container);
                }
            }
            if(isFCL)
                containerDao.saveAll(containersList);
        }
    }

    private void validateContainers(ShipmentDetails shipmentDetails, Containers container) {
        boolean isPart = container.getIsPart() != null && container.getIsPart().booleanValue();
        if((shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL) || !isPart) && container.getShipmentsList() != null && !container.getShipmentsList().isEmpty()) {
            String errorMsg = "This container is already linked to another shipment. Only part Container/Containers are allowed to attach";
            if(isPart)
                errorMsg = "Mentioned container " + container.getContainerNumber() + " is already assigned to a Shipment - " + container.getShipmentsList().iterator().next().getShipmentId() + ". Please check and retry.";
            throw new ValidationException(errorMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> assignAllContainers(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        boolean lclAndSeaOrRoadFlag = shipmentSettingsDetails.getMultipleShipmentEnabled() != null && shipmentSettingsDetails.getMultipleShipmentEnabled();
        boolean isConsolidatorFlag = shipmentSettingsDetails.getIsConsolidator() != null && shipmentSettingsDetails.getIsConsolidator();
        List<Containers> containersList = new ArrayList<>();
        try {
            ContainerAssignListRequest containerAssignRequest = (ContainerAssignListRequest) commonRequestModel.getData();
            Long shipmentId = containerAssignRequest.getShipmentId();
            Long consolidationId = containerAssignRequest.getConsolidationId();
            if (lclAndSeaOrRoadFlag && !containerAssignRequest.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) && !containerAssignRequest.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA)) {
                    lclAndSeaOrRoadFlag = false;
            }

            ListCommonRequest listCommonRequest = constructListCommonRequest(CONSOLIDATION_ID, consolidationId, "=");
            Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
            Page<Containers> containers = containerDao.findAll(pair.getLeft(), pair.getRight());
            Map<Long, Containers> containersMap = containers.getContent().stream().collect(Collectors.toMap(e -> e.getId(), c -> c));
            ShipmentDetails shipmentDetails = shipmentDao.findById(containerAssignRequest.getShipmentId()).get();
            Set<Containers> oldContainers = shipmentDetails.getContainersList();
            List<Long> containerIds = getContainerIds(lclAndSeaOrRoadFlag, containers, shipmentId, containersList, isConsolidatorFlag, shipmentDetails);
            if(!Objects.isNull(containerIds) && !containerIds.isEmpty()) {
                shipmentsContainersMappingDao.assignContainers(containerAssignRequest.getShipmentId(), containerIds, shipmentDetails.getGuid().toString());
                makeShipmentsDG(containersMap, shipmentDetails);
            }
            dependentServiceHelper.pushShipmentDataToDependentService(shipmentDetails, false, false, oldContainers);
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private List<Long> getContainerIds(boolean lclAndSeaOrRoadFlag, Page<Containers> containers, Long shipmentId, List<Containers> containersList, boolean isConsolidatorFlag, ShipmentDetails shipmentDetails) throws RunnerException {
        List<Containers> conts = new ArrayList<>();
        List<Long> containerIds = new ArrayList<>();
        if(lclAndSeaOrRoadFlag) {
            addContainerListWithContAndContent(containers, shipmentId, containersList, isConsolidatorFlag, conts);
            saveContainersListIfFcl(shipmentDetails, containersList, containerIds);
        }
        else {
            for (Containers container : containers.getContent()) {
                List<ShipmentsContainersMapping> shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerId(container.getId());
                if(shipmentsContainersMappings.isEmpty()) {
                    containerIds.add(container.getId());
                }
            }
        }
        return containerIds;
    }

    private void addContainerListWithContAndContent(Page<Containers> containers, Long shipmentId, List<Containers> containersList, boolean isConsolidatorFlag, List<Containers> conts) throws RunnerException {
        for (Containers container : containers.getContent()) {
            List<ShipmentsContainersMapping> shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerId(container.getId());
            if(!shipmentsContainersMappings.stream().map(ShipmentsContainersMapping::getShipmentId).toList().contains(shipmentId)) {

                if(container.getAllocatedWeight() != null && container.getAchievedWeight() != null && container.getAllocatedVolume() != null
                        && isNotEmpty(container.getAllocatedWeightUnit()) && isNotEmpty(container.getAllocatedVolumeUnit()) && isNotEmpty(container.getAchievedWeightUnit()) && isNotEmpty(container.getAchievedVolumeUnit())) {

                    BigDecimal achievedWeight = new BigDecimal(convertUnit(Constants.MASS, container.getAchievedWeight(), container.getAchievedWeightUnit(), container.getAllocatedWeightUnit()).toString());
                    BigDecimal achievedVolume = new BigDecimal(convertUnit(Constants.VOLUME, container.getAchievedVolume(), container.getAchievedVolumeUnit(), container.getAllocatedVolumeUnit()).toString());

                    if(achievedWeight.compareTo(container.getAllocatedWeight()) < 0 && achievedVolume.compareTo(container.getAllocatedVolume()) < 0) {
                        containersList.add(container);
                    }
                    else if(!isConsolidatorFlag) {
                        conts.add(container);
                    }
                }
                else
                    containersList.add(container);
            }
        }
        addContainersListForConts(containersList, conts);
    }

    private void addContainersListForConts(List<Containers> containersList, List<Containers> conts) {
        if(!conts.isEmpty()) {
            for (Containers x : conts) {
                boolean flag = getFlagForShipmentList(x);
                if (flag)
                    containersList.add(x);
            }
        }
    }

    private boolean getFlagForShipmentList(Containers x) {
        boolean flag = true;
        if(x.getShipmentsList() != null && !x.getShipmentsList().isEmpty()) {
            for(ShipmentDetails shipmentDetails1 : x.getShipmentsList()) {
                if(shipmentDetails1.getShipmentType().equals(Constants.CARGO_TYPE_FCL))
                    flag = false;
            }
        }
        return flag;
    }

    private void saveContainersListIfFcl(ShipmentDetails shipmentDetails, List<Containers> containersList, List<Long> containerIds) {
        boolean isFCL = shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL) && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA));
        for (Containers container : containersList) {
            boolean isPart = container.getIsPart() != null && container.getIsPart().booleanValue();
            if ((!shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL) && isPart) || setIsNullOrEmpty(container.getShipmentsList())) {
                containerIds.add(container.getId());
            }
            if (isFCL) {
                container.setAchievedWeight(container.getAllocatedWeight());
                container.setAchievedVolume(container.getAllocatedVolume());
                container.setAchievedWeightUnit(container.getAllocatedWeightUnit());
                container.setAchievedVolumeUnit(container.getAllocatedVolumeUnit());
                container.setWeightUtilization("100");
                container.setVolumeUtilization("100");
            }
        }
        if(isFCL)
            containerDao.saveAll(containersList);
    }

    private void saveDGShipment(ShipmentDetails shipmentDetails, boolean isDGClass1Added) throws RunnerException {
        shipmentDao.entityDetach(List.of(shipmentDetails));
        Optional<ShipmentDetails> optionalShipmentDetails = shipmentDao.findById(shipmentDetails.getId());
        if(!optionalShipmentDetails.isPresent())
            return;
        shipmentDetails = optionalShipmentDetails.get();
        boolean saveShipment = !Boolean.TRUE.equals(shipmentDetails.getContainsHazardous());
        shipmentDetails.setContainsHazardous(true);
        saveShipment = saveShipment || commonUtils.changeShipmentDGStatusToReqd(shipmentDetails, isDGClass1Added);
        if(saveShipment) {
            shipmentDetails = shipmentDao.save(shipmentDetails, false);
            shipmentSync.sync(shipmentDetails, null, null, shipmentDetails.getGuid().toString(), false);
        }
    }

    public void makeShipmentsDG(Map<Long, Containers> containersMap, ShipmentDetails shipmentDetails) throws RunnerException {
        boolean isDG = false;
        boolean isDGClass1Added = false;
        for(Map.Entry<Long, Containers> map : containersMap.entrySet()) {
            if(Boolean.TRUE.equals(map.getValue().getHazardous())) {
                isDGClass1Added = isDGClass1Added || commonUtils.checkIfDGClass1(map.getValue().getDgClass());
                isDG = true;
            }
        }
        if(isDG)
            saveDGShipment(shipmentDetails, isDGClass1Added);
    }

    private List<IRunnerResponse> convertEntityListToFullShipmentList(List<ShipmentDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(shipmentDetail -> {
            ShipmentDetailsResponse response = modelMapper.map(shipmentDetail, ShipmentDetailsResponse.class);
            setShipperReferenceNumber(response);
            responseList.add(response);
        });
        return responseList;
    }

    private void setShipperReferenceNumber(ShipmentDetailsResponse response){
        if(response.getReferenceNumbersList() != null && !response.getReferenceNumbersList().isEmpty()){
           Optional<String> srnReferenceNumber = response.getReferenceNumbersList().stream()
                .filter(i -> i.getType().equalsIgnoreCase(SRN))
                .findFirst()
                .map(a -> a.getReferenceNumber());

           if(srnReferenceNumber.isPresent() && response.getPickupDetails() != null){
               response.getPickupDetails().setShipperRef(srnReferenceNumber.get());
           }
        }
    }

    private void setShipperReferenceNumber(ShipmentListResponse response){
        if(response.getReferenceNumbersList() != null && !response.getReferenceNumbersList().isEmpty()){
            Optional<String> srnReferenceNumber = response.getReferenceNumbersList().stream()
                .filter(i -> i.getType().equalsIgnoreCase(SRN))
                .findFirst()
                .map(a -> a.getReferenceNumber());

            if(srnReferenceNumber.isPresent() && response.getPickupDetails() != null){
                response.getPickupDetails().setShipperRef(srnReferenceNumber.get());
            }
        }
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        return list(commonRequestModel, false);
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel, boolean getMasterData) {
        String responseMsg;
        int totalPage = 0;
        long totalElements = 0;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(ShipmentConstants.SHIPMENT_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException(ShipmentConstants.SHIPMENT_LIST_REQUEST_NULL_ERROR);
            }
            if(Boolean.TRUE.equals(request.getNotificationFlag())) {
                Page<Long> eligibleShipmentId = shipmentDao.getIdWithPendingActions(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED,
                    PageRequest.of(Math.max(0,request.getPageNo()-1), request.getPageSize()));

                List<Long> shipmentIds = notificationDao.findEntityIdsByEntityType(SHIPMENT);

                Set<Long> uniqueShipmentIds = new HashSet<>(eligibleShipmentId.getContent());
                uniqueShipmentIds.addAll(shipmentIds);

                List<Long> combinedShipmentIds = new ArrayList<>(uniqueShipmentIds);

                andCriteria("id", combinedShipmentIds, "IN", request);

                totalElements = combinedShipmentIds.size();
                int pageSize = request.getPageSize();
                totalPage = (int) ((totalElements + pageSize - 1) / pageSize);
            }
            checkWayBillNumberCriteria(request);
            log.info(ShipmentConstants.SHIPMENT_LIST_CRITERIA_PREPARING, LoggerHelper.getRequestIdFromMDC());
            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());

            log.info(ShipmentConstants.SHIPMENT_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());
            if(!Boolean.TRUE.equals(request.getNotificationFlag())) {
                totalPage = shipmentDetailsPage.getTotalPages();
                totalElements = shipmentDetailsPage.getTotalElements();
            }
            if(request.getIncludeColumns() == null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildListSuccessResponse(
                        convertEntityListToDtoList(shipmentDetailsPage.getContent(), getMasterData),
                        totalPage,
                        totalElements);
            else {
                List<IRunnerResponse>filteredList = new ArrayList<>();
                for( var curr: convertEntityListToDtoList(shipmentDetailsPage.getContent(), getMasterData)){
                    RunnerPartialListResponse res=new RunnerPartialListResponse();
                    res.setData(partialFetchUtils.fetchPartialListData(curr,request.getIncludeColumns()));
                    filteredList.add(res);

                }
                return ResponseHelper.buildListSuccessResponse(
                        filteredList,
                        totalPage,
                        totalElements);
            }
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }

    public Page<ShipmentDetails> findAllWithOutIncludeColumn(Specification<ShipmentDetails> spec, Pageable pageable) {
        var start = System.currentTimeMillis();
        var shipmentList = shipmentDao.findAll(spec, pageable);

        List<Long> partyIds = shipmentList.stream().map(ShipmentDetails::getClientId).collect(Collectors.toList());
        partyIds.addAll(shipmentList.stream().map(ShipmentDetails::getConsigneeId).collect(Collectors.toList()));
        partyIds.addAll(shipmentList.stream().map(ShipmentDetails::getConsignerId).collect(Collectors.toList()));

        Map<Long, Parties> partyMap = partiesDao.findByIds(partyIds.stream().filter(Objects::nonNull).collect(Collectors.toList()))
                .stream().collect(Collectors.toMap(Parties::getId, Function.identity()));

        shipmentList.forEach(c -> c.setClient(partyMap.get(c.getClientId())));
        shipmentList.forEach(c -> c.setConsignee(partyMap.get(c.getConsigneeId())));
        shipmentList.forEach(c -> c.setConsigner(partyMap.get(c.getConsignerId())));

        List<Long> pickupDetailsId = new ArrayList<>();
        pickupDetailsId.addAll(shipmentList.stream().map(ShipmentDetails::getPickupDetailsId).collect(Collectors.toList()));
        pickupDetailsId.addAll(shipmentList.stream().map(ShipmentDetails::getDeliveryDetailsId).collect(Collectors.toList()));
        if(!pickupDetailsId.isEmpty()) {
            Map<Long, PickupDeliveryDetails> pickupDetailsMap = pickupDeliveryDetailsDao.findByIdIn(pickupDetailsId.stream().filter(Objects::nonNull).collect(Collectors.toList()))
                    .stream().collect(Collectors.toMap(PickupDeliveryDetails::getId, Function.identity()));

            shipmentList.forEach(c -> c.setPickupDetails(pickupDetailsMap.get(c.getPickupDetailsId())));
            shipmentList.forEach(c -> c.setDeliveryDetails(pickupDetailsMap.get(c.getDeliveryDetailsId())));
        }

        log.info("{} | findAllWithOutIncludeColumn: {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - start);
        return shipmentList;
    }

    private void checkWayBillNumberCriteria(ListCommonRequest request)
    {
        if(request != null && request.getFilterCriteria() != null && !request.getFilterCriteria().isEmpty())
        {
            checkForWayBillFilter(request.getFilterCriteria());
        }
    }

    private void checkForWayBillFilter(List<FilterCriteria> filterCriteriaList) {
        for(FilterCriteria filterCriteria: filterCriteriaList)
        {
            if(filterCriteria.getCriteria() != null && filterCriteria.getCriteria().getFieldName() != null &&
                    filterCriteria.getCriteria().getFieldName().equals("wayBillNumber") && filterCriteria.getCriteria().getValue() != null) {

                WayBillNumberFilterRequest wayBillNumberFilterRequest = new WayBillNumberFilterRequest();
                wayBillNumberFilterRequest.setWayBillNumber(filterCriteria.getCriteria().getValue().toString());
                GuidsListResponse guidsListResponse = v1Service.fetchWayBillNumberFilterGuids(wayBillNumberFilterRequest);
                filterCriteria.getCriteria().setFieldName("guid");
                filterCriteria.getCriteria().setOperator("IN");
                filterCriteria.getCriteria().setValue(guidsListResponse.getGuidsList());
            }
            if(filterCriteria.getInnerFilter() != null && !filterCriteria.getInnerFilter().isEmpty()) {
                checkForWayBillFilter(filterCriteria.getInnerFilter());
            }
        }
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Shipment async list for Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException(ShipmentConstants.SHIPMENT_LIST_REQUEST_NULL_ERROR);
            }
            request.setIncludeTbls(Arrays.asList(Constants.ADDITIONAL_DETAILS, Constants.CLIENT, Constants.CONSIGNER, Constants.CONSIGNEE, Constants.CARRIER_DETAILS));
            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Shipment async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                    shipmentDetailsPage.getTotalPages(),
                    shipmentDetailsPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }

    }

    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.debug("Request is empty for Shipment delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.debug("Request Id is null for Shipment delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
            if (!shipmentDetails.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            String oldEntityJsonString = jsonHelper.convertToJson(shipmentDetails.get());
            shipmentDao.delete(shipmentDetails.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, ShipmentDetails.class))
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(shipmentDetails.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );

            log.info("Deleted Shipment details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public boolean isNotAllowedToViewShipment(List<TriangulationPartner> triangulationPartners,
                                              ShipmentDetails shipmentDetails, Long currentTenant,
                                              ConsolidationDetails consolidationDetails) {
        boolean isNotAllowed = true;
        if(consolidationDetails!=null && Objects.equals(consolidationDetails.getReceivingBranch(), currentTenant))
            isNotAllowed = false;

        if(consolidationDetails!=null && consolidationDetails.getTriangulationPartnerList() != null && consolidationDetails.getTriangulationPartnerList().stream().filter(Objects::nonNull)
                .anyMatch(tp -> Objects.equals(tp.getTriangulationPartner(), currentTenant)))
            isNotAllowed = false;

        if(consolidationDetails!=null && Objects.equals(consolidationDetails.getTriangulationPartner(), currentTenant))
            isNotAllowed = false;

        if(Objects.equals(shipmentDetails.getReceivingBranch(), currentTenant))
            isNotAllowed = false;

        if(triangulationPartners != null && triangulationPartners.stream().filter(Objects::nonNull)
                .anyMatch(tp -> Objects.equals(tp.getTriangulationPartner(), currentTenant)))
            isNotAllowed = false;

        if(Objects.equals(shipmentDetails.getTriangulationPartner(), currentTenant))
            isNotAllowed = false;

        return isNotAllowed;
    }


    public ResponseEntity<IRunnerResponse> retrieveForNTE(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            double start = System.currentTimeMillis();
            if(request.getId() == null && request.getGuid() == null) {
                log.error(ShipmentConstants.SHIPMENT_ID_GUID_NULL_FOR_RETRIEVE_NTE, LoggerHelper.getRequestIdFromMDC());
                throw new RunnerException(ShipmentConstants.ID_GUID_NULL_ERROR);
            }
            Long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails;
            if(id != null){
                shipmentDetails = shipmentDao.findShipmentByIdWithQuery(id);
            }
            else {
                UUID guid = UUID.fromString(request.getGuid());
                shipmentDetails = shipmentDao.findShipmentByGuidWithQuery(guid);
            }
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for the input with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            List<TriangulationPartner> triangulationPartners = shipmentDetails.get().getTriangulationPartnerList();
            Long currentTenant = TenantContext.getCurrentTenant().longValue();
            ConsolidationDetails consolidationDetails = null;
            if (!CommonUtils.setIsNullOrEmpty(shipmentDetails.get().getConsolidationList())) {
                consolidationDetails = shipmentDetails.get().getConsolidationList().iterator().next();
            }
            if (isNotAllowedToViewShipment(triangulationPartners, shipmentDetails.get(), currentTenant, consolidationDetails)) {
                throw new AuthenticationException(Constants.NOT_ALLOWED_TO_VIEW_SHIPMENT_FOR_NTE);
            }
            List<Notes> notes = notesDao.findByEntityIdAndEntityType(request.getId(), Constants.CUSTOMER_BOOKING);
            double current = System.currentTimeMillis();
            log.info("Shipment details fetched successfully for Id {} with Request Id {} within: {}ms", id, LoggerHelper.getRequestIdFromMDC(), current - start);
            ShipmentDetailsResponse response = modelMapper.map(shipmentDetails.get(), ShipmentDetailsResponse.class);
            if (response.getStatus() != null && response.getStatus() < ShipmentStatus.values().length)
                response.setShipmentStatus(ShipmentStatus.values()[response.getStatus()].toString());
            log.info("Request: {} || Time taken for model mapper: {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - current);
            response.setCustomerBookingNotesList(jsonHelper.convertValueToList(notes,NotesResponse.class));
            createShipmentPayload(shipmentDetails.get(), response, true);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        return retrieveById(commonRequestModel, false);
    }

    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel, boolean getMasterData) {
        String responseMsg;
        try {
            return ResponseHelper.buildSuccessResponse(retireveShipmentData(commonRequestModel, false, false));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ShipmentDetailsResponse retireveShipmentData(CommonRequestModel commonRequestModel, boolean measurmentBasis, boolean getMasterData) throws RunnerException {
        CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
        double start = System.currentTimeMillis();
        if(request.getId() == null && request.getGuid() == null) {
            log.error(ShipmentConstants.SHIPMENT_ID_GUID_NULL_FOR_RETRIEVE_NTE, LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException(ShipmentConstants.ID_GUID_NULL_ERROR);
        }
        Long id = request.getId();
        Optional<ShipmentDetails> shipmentDetails;
        if(id != null ){
            shipmentDetails = shipmentDao.findById(id);
        } else {
            UUID guid = UUID.fromString(request.getGuid());
            shipmentDetails = shipmentDao.findByGuid(guid);
        }
        if (!shipmentDetails.isPresent()) {
            log.debug("Shipment Details is null for the input with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        double current = System.currentTimeMillis();
        log.info("Shipment details fetched successfully for Id {} with Request Id {} within: {}ms", id, LoggerHelper.getRequestIdFromMDC(), current - start);
        AtomicInteger pendingCount = new AtomicInteger(0);
        Long shipmentId = shipmentDetails.get().getId();
        UUID guid = shipmentDetails.get().getGuid();
        List<NotesResponse> notesResponses = new ArrayList<>();
        List<String> implications = new ArrayList<>();
        var pendingNotificationFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> setPendingCount(shipmentId, pendingCount)), executorService);
        var notesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> setNotesResponse(request.getId(), notesResponses)), executorService);
        var implicationListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> setImplicationsResponse(guid, implications)), executorService);
        ShipmentDetailsResponse response = modelMapper.map(shipmentDetails.get(), ShipmentDetailsResponse.class);
        log.info("Request: {} || Time taken for model mapper: {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - current);
        CompletableFuture.allOf(pendingNotificationFuture, notesFuture, implicationListFuture).join();
        if (response.getStatus() != null && response.getStatus() < ShipmentStatus.values().length)
            response.setShipmentStatus(ShipmentStatus.values()[response.getStatus()].toString());
        response.setPendingActionCount((pendingCount.get() == 0) ? null : pendingCount.get());
        response.setCustomerBookingNotesList(notesResponses);
        // set dps implications
        response.setImplicationList(implications);

        if(measurmentBasis) {
            calculatePacksAndPacksUnit(shipmentDetails.get().getPackingList(), response);
        } else {
            createShipmentPayload(shipmentDetails.get(), response, getMasterData);
        }
        return response;
    }

    private void setImplicationsResponse(UUID guid, List<String> implications) {
        implications.addAll(dpsEventService.getImplicationsForShipment(guid.toString()));
    }

    private void setNotesResponse(Long id,  List<NotesResponse> notesResponses) {
        List<Notes> notes = notesDao.findByEntityIdAndEntityType(id, Constants.CUSTOMER_BOOKING);
        notesResponses.addAll(jsonHelper.convertValueToList(notes,NotesResponse.class));
    }

    private void setPendingCount(Long shipmentId, AtomicInteger pendingCount) {
        var map = consoleShipmentMappingDao.pendingStateCountBasedOnShipmentId(Arrays.asList(shipmentId), ShipmentRequestedType.SHIPMENT_PULL_REQUESTED.ordinal());
        var notificationMap = notificationDao.pendingNotificationCountBasedOnEntityIdsAndEntityType(Arrays.asList(shipmentId), SHIPMENT);
        int value = map.getOrDefault(shipmentId, 0) + notificationMap.getOrDefault(shipmentId, 0);
        pendingCount.set(value);
    }

    public ResponseEntity<IRunnerResponse> retrieveByIdWithBookingNotes(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Shipment async retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null && request.getGuid() == null) {
                log.error("Request Id is null for Shipment async retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails;
            if(request.getId() != null ){
                shipmentDetails = shipmentDao.findById(id);
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                shipmentDetails = shipmentDao.findByGuid(guid);
            }
            if (!shipmentDetails.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            shipmentDetails.get().setNotesList(notesDao.findByEntityIdAndEntityType(id, Constants.SHIPMENT));
            log.info("Shipment details async fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            ShipmentDetailsResponse response = jsonHelper.convertValue(shipmentDetails.get(), ShipmentDetailsResponse.class);
            response.setCustomerBookingNotesList(commonUtils.convertToDtoList(notesDao.findByEntityIdAndEntityType(request.getId(), Constants.CUSTOMER_BOOKING),NotesResponse.class));
            this.fetchNTEstatusForReceivingBranch(response);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> completeRetrieveById(CommonRequestModel commonRequestModel) throws ExecutionException, InterruptedException {
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request.getId() == null && request.getGuid() == null) {
                log.error(ShipmentConstants.SHIPMENT_ID_GUID_NULL_FOR_RETRIEVE_NTE, LoggerHelper.getRequestIdFromMDC());
                throw new RunnerException(ShipmentConstants.ID_GUID_NULL_ERROR);
            }
            ResponseEntity<IRunnerResponse> shipmentsRespone = retrieveByIdWithBookingNotes(commonRequestModel);
            RunnerResponse<ShipmentDetailsResponse> res = (RunnerResponse<ShipmentDetailsResponse>) shipmentsRespone.getBody();
            if(request.getIncludeColumns() == null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildSuccessResponse(res.getData());
            else
                return ResponseHelper.buildSuccessResponse(partialFetchUtils.fetchPartialData(res, request.getIncludeColumns()));
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> partialUpdate(CommonRequestModel commonRequestModel, Boolean fromV1) throws RunnerException {

        ShipmentPatchRequest shipmentRequest = (ShipmentPatchRequest) commonRequestModel.getData();
        if ((shipmentRequest.getId() == null && shipmentRequest.getGuid() == null) && (shipmentRequest.getShipmentId() == null || "".equals(shipmentRequest.getShipmentId().get()))) {
            log.error("Request Id is null for update request with Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException("Request Id is null");
        }
        List<ContainerRequest> containerRequestList = shipmentRequest.getContainersList();
        AdditionalDetailRequest additionalDetailRequest = shipmentRequest.getAdditionalDetail();
        CarrierPatchRequest carrierDetailRequest =  shipmentRequest.getCarrierDetails();

        Long id = null;
        Optional<ShipmentDetails> oldShipmentDetails;
        ShipmentRequest fetchShipmentRequest = new ShipmentRequest();
        fetchShipmentRequest.setId(shipmentRequest.getId() != null ? shipmentRequest.getId().get() : null);
        fetchShipmentRequest.setGuid(shipmentRequest.getGuid());
        if(shipmentRequest.getId() != null || shipmentRequest.getGuid() != null) {
            oldShipmentDetails = retrieveByIdOrGuid(fetchShipmentRequest);
            id = oldShipmentDetails.get().getId();
        }
        else {
            List<ShipmentDetails> shipmentDetails = shipmentDao.findByShipmentIdIn(List.of(shipmentRequest.getShipmentId().get()));
            if(!CollectionUtils.isEmpty(shipmentDetails) && shipmentDetails.size() == 1) {
                oldShipmentDetails = Optional.of(shipmentDetails.get(0));
                id = oldShipmentDetails.get().getId();
            }
            else if(CollectionUtils.isEmpty(shipmentDetails)) {
                log.error("Shipment not available for update request with Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            else {
                log.error("More than one shipments available for update request with Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INCORRECT_RESULT_SIZE_EXCEPTION_MSG);
            }
        }
        if (!oldShipmentDetails.isPresent()) {
            log.debug(ShipmentConstants.SHIPMENT_DETAILS_NULL_FOR_ID_ERROR, shipmentRequest.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        return getPartialUpdateResponse(fromV1, oldShipmentDetails, shipmentRequest, containerRequestList, id, additionalDetailRequest, carrierDetailRequest);
    }

    private ResponseEntity<IRunnerResponse> getPartialUpdateResponse(Boolean fromV1, Optional<ShipmentDetails> oldShipmentDetails, ShipmentPatchRequest shipmentRequest, List<ContainerRequest> containerRequestList, Long id, AdditionalDetailRequest additionalDetailRequest, CarrierPatchRequest carrierDetailRequest) throws RunnerException {
        try {
            ShipmentDetails newShipmentDetails = oldShipmentDetails.get();

            ShipmentDetails oldEntity = jsonHelper.convertValue(newShipmentDetails, ShipmentDetails.class);
            shipmentDetailsMapper.update(shipmentRequest, newShipmentDetails);
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            newShipmentDetails.setId(oldShipmentDetails.get().getId());
            Long consolidationId = null;
            if (ObjectUtils.isNotEmpty(newShipmentDetails.getConsolidationList())) {
                consolidationId = newShipmentDetails.getConsolidationList().iterator().next().getId();
            }
            Set<Containers> updatedContainers = null;
            if (containerRequestList != null) {
                updatedContainers = new HashSet<>(containerDao.updateEntityFromShipmentConsole(commonUtils.convertToEntityList(containerRequestList, Containers.class),
                                                  consolidationId, id, false));
            } else {
                updatedContainers = oldShipmentDetails.get().getContainersList();
            }
            newShipmentDetails.setContainersList(updatedContainers);
            AdditionalDetails updatedAdditionalDetails = null;
            if (additionalDetailRequest != null) {
                updatedAdditionalDetails = additionalDetailDao.updateEntityFromShipment(jsonHelper.convertValue(additionalDetailRequest, AdditionalDetails.class));
                newShipmentDetails.setAdditionalDetails(updatedAdditionalDetails);
            }
            CarrierDetails updatedCarrierDetails = null;
            if (carrierDetailRequest != null) {
                updatedCarrierDetails = oldShipmentDetails.get().getCarrierDetails();
                carrierDetailsMapper.update(carrierDetailRequest, updatedCarrierDetails);
                newShipmentDetails.setCarrierDetails(oldShipmentDetails.get().getCarrierDetails());
            }
            newShipmentDetails.setCarrierDetails(oldShipmentDetails.get().getCarrierDetails());
            validateBeforeSave(newShipmentDetails);

            ConsolidationDetails consolidationDetails = updateLinkedShipmentData(newShipmentDetails, oldShipmentDetails.get(), null);
            if(!Objects.isNull(consolidationDetails)) {
                newShipmentDetails.setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails)));
            }
            newShipmentDetails = shipmentDao.update(newShipmentDetails, false);

            newShipmentDetails.setContainersList(updatedContainers);
            if (additionalDetailRequest != null) {
                newShipmentDetails.setAdditionalDetails(updatedAdditionalDetails);
            }
            if (carrierDetailRequest != null) {
                newShipmentDetails.setCarrierDetails(updatedCarrierDetails);
            }
            processListTypeRequests(id, newShipmentDetails, oldEntity, shipmentSettingsDetails, shipmentRequest);

            syncShipmentInV1(fromV1, newShipmentDetails, consolidationDetails);

            dependentServiceHelper.pushShipmentDataToDependentService(newShipmentDetails, false, false, oldShipmentDetails.get().getContainersList());
            ShipmentDetailsResponse response = shipmentDetailsMapper.map(newShipmentDetails);
            triggerAsyncTasks(newShipmentDetails, shipmentSettingsDetails, oldEntity);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
    }

    private void processListTypeRequests(Long id, ShipmentDetails newShipmentDetails, ShipmentDetails oldEntity, ShipmentSettingsDetails shipmentSettingsDetails, ShipmentPatchRequest shipmentRequest) throws RunnerException {
        List<BookingCarriageRequest> bookingCarriageRequestList = shipmentRequest.getBookingCarriagesList();
        List<TruckDriverDetailsRequest> truckDriverDetailsRequestList = shipmentRequest.getTruckDriverDetails();
        List<PackingRequest> packingRequestList = shipmentRequest.getPackingList();
        List<ELDetailsRequest> elDetailsRequestList = shipmentRequest.getElDetailsList();
        List<EventsRequest> eventsRequestList = shipmentRequest.getEventsList();
        List<NotesRequest> notesRequestList = shipmentRequest.getNotesList();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = shipmentRequest.getReferenceNumbersList();
        List<RoutingsRequest> routingsRequestList = shipmentRequest.getRoutingsList();
        List<ServiceDetailsRequest> serviceDetailsRequestList = shipmentRequest.getServicesList();
        List<ShipmentOrderRequest> shipmentOrderRequestList = shipmentRequest.getShipmentOrders();

        if (bookingCarriageRequestList != null) {
            List<BookingCarriage> updatedBookingCarriages = bookingCarriageDao.updateEntityFromShipment(jsonHelper.convertValueToList(bookingCarriageRequestList, BookingCarriage.class), id);
            newShipmentDetails.setBookingCarriagesList(updatedBookingCarriages);
        }
        if (truckDriverDetailsRequestList != null) {
            List<TruckDriverDetails> updatedTruckDriverDetails = truckDriverDetailsDao.updateEntityFromShipment(jsonHelper.convertValueToList(truckDriverDetailsRequestList, TruckDriverDetails.class), id);
            newShipmentDetails.setTruckDriverDetails(updatedTruckDriverDetails);
        }
        if (shipmentOrderRequestList != null) {
            List<ShipmentOrder> updatedShipmentOrders = shipmentOrderDao.updateEntityFromShipment(jsonHelper.convertValueToList(shipmentOrderRequestList, ShipmentOrder.class), id);
            newShipmentDetails.setShipmentOrders(updatedShipmentOrders);
        }
        if (packingRequestList != null) {
            List<Packing> updatedPackings = packingDao.updateEntityFromShipment(jsonHelper.convertValueToList(packingRequestList, Packing.class), id, null);
            newShipmentDetails.setPackingList(updatedPackings);
        }
        if (elDetailsRequestList != null) {
            List<ELDetails> updatedELDetails = elDetailsDao.updateEntityFromShipment(jsonHelper.convertValueToList(elDetailsRequestList, ELDetails.class), id);
            newShipmentDetails.setElDetailsList(updatedELDetails);
        }
        if (eventsRequestList != null) {
            List<Events> eventsList = jsonHelper.convertValueToList(eventsRequestList, Events.class);
            eventsList = createOrUpdateEvents(newShipmentDetails, oldEntity, eventsList, false);
            if (eventsList != null) {
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(eventsList, id, Constants.SHIPMENT);
                newShipmentDetails.setEventsList(updatedEvents);
                eventService.updateAtaAtdInShipment(updatedEvents, newShipmentDetails, shipmentSettingsDetails);
            }
        }
        // Create events on basis of shipment status Confirmed/Created
        autoGenerateEvents(newShipmentDetails);

        if (notesRequestList != null) {
            List<Notes> updatedNotes = notesDao.updateEntityFromOtherEntity(jsonHelper.convertValueToList(notesRequestList, Notes.class), id, Constants.SHIPMENT);
            newShipmentDetails.setNotesList(updatedNotes);
        }
        if (referenceNumbersRequestList != null) {
            List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(jsonHelper.convertValueToList(referenceNumbersRequestList, ReferenceNumbers.class), id);
            newShipmentDetails.setReferenceNumbersList(updatedReferenceNumbers);
        }
        if (routingsRequestList != null) {
            List<Routings> updatedRoutings = routingsDao.updateEntityFromShipment(jsonHelper.convertValueToList(routingsRequestList, Routings.class), id);
            newShipmentDetails.setRoutingsList(updatedRoutings);
        }
        if (serviceDetailsRequestList != null) {
            List<ServiceDetails> updatedServiceDetails = serviceDetailsDao.updateEntityFromShipment(jsonHelper.convertValueToList(serviceDetailsRequestList, ServiceDetails.class), id);
            newShipmentDetails.setServicesList(updatedServiceDetails);
        }
    }

    private void syncShipmentInV1(Boolean fromV1, ShipmentDetails newShipmentDetails, ConsolidationDetails consolidationDetails) {
        if(fromV1 == null || !fromV1) {
            syncShipment(newShipmentDetails, null, null, null, consolidationDetails, true);
        }
    }

    private void triggerAsyncTasks(ShipmentDetails newShipmentDetails, ShipmentSettingsDetails shipmentSettingsDetails, ShipmentDetails oldEntity) {
        ShipmentDetails newShipment = newShipmentDetails;
        if (commonUtils.getCurrentTenantSettings().getP100Branch() != null && commonUtils.getCurrentTenantSettings().getP100Branch())
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> bookingIntegrationsUtility.updateBookingInPlatform(newShipment)), executorService);
        if(Boolean.TRUE.equals(shipmentSettingsDetails.getIsNetworkTransferEntityEnabled()))
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> createOrUpdateNetworkTransferEntity(newShipment, oldEntity)), executorService);
        if(Boolean.TRUE.equals(shipmentSettingsDetails.getIsAutomaticTransferEnabled()))
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> triggerAutomaticTransfer(newShipment, oldEntity, false)), executorService);
    }

    public ResponseEntity<IRunnerResponse> toggleLock(CommonRequestModel commonRequestModel) throws RunnerException {
        CommonGetRequest commonGetRequest = (CommonGetRequest) commonRequestModel.getData();
        Long id = commonGetRequest.getId();
        Optional<ShipmentDetails> optionalShipmentDetails = shipmentDao.findById(id);
        if (optionalShipmentDetails.isEmpty()) {
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ShipmentDetails shipmentDetails = optionalShipmentDetails.get();
        String lockingUser = shipmentDetails.getLockedBy();
        String currentUser = UserContext.getUser().getUsername();

        if (shipmentDetails.getIsLocked() != null && shipmentDetails.getIsLocked()) {
            if (lockingUser != null && (Objects.equals(lockingUser, currentUser) ||
                    (!Objects.isNull(PermissionsContext.getPermissions(PermissionConstants.TENANT_SUPER_ADMIN)) && !PermissionsContext.getPermissions(PermissionConstants.TENANT_SUPER_ADMIN).isEmpty()) ))
                shipmentDetails.setIsLocked(false);
            else
                throw new RunnerException(String.format(ErrorConstants.LOCK_UNLOCK_ERROR, Constants.SHIPMENT_CAMELCASE, lockingUser));
        } else {
            shipmentDetails.setIsLocked(true);
            shipmentDetails.setLockedBy(currentUser);
        }
        shipmentDetails = shipmentDao.save(shipmentDetails, false);
        shipmentSync.syncLockStatus(shipmentDetails);
        dependentServiceHelper.pushShipmentDataToDependentService(shipmentDetails, false, false, shipmentDetails.getContainersList());
        return ResponseHelper.buildSuccessResponse();
    }

    private String generateShipmentId(ShipmentDetails shipmentDetails) {
        Optional<ShipmentSettingsDetails> shipmentSettingsOptional = shipmentSettingsDao.findByTenantId(TenantContext.getCurrentTenant());
        String shipmentId = "";
        boolean flag = true;
        int counter = 1;
        while(flag) {
            ListCommonRequest listRequest = constructListCommonRequest(Constants.SHIPMENT_ID, shipmentId, "=");
            Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listRequest, ShipmentDetails.class);
            Page<ShipmentDetails> shipments = shipmentDao.findAll(pair.getLeft(), pair.getRight());

            if(!shipmentId.isEmpty() && shipments.getTotalElements() == 0)
                flag = false;
            else {
                log.info("CR-ID {} || Inside generateShipmentId: with shipmentID: {} | counter: {}", LoggerHelper.getRequestIdFromMDC(), shipmentId, counter++);
                if(shipmentSettingsOptional.isPresent() && Boolean.TRUE.equals(shipmentSettingsOptional.get().getCustomisedSequence())) {
                    try{
                        shipmentId = getCustomizedShipmentProcessNumber(ProductProcessTypes.ShipmentNumber, shipmentDetails);
                    } catch (Exception ignored) {
                        log.error("Execption during common sequence {}", ignored.getMessage());
                        log.error("Execption occurred for common sequence {}", ignored.getStackTrace());
                        shipmentId = Constants.SHIPMENT_ID_PREFIX + getShipmentsSerialNumber();
                    }
                }
                shipmentId = getShipmentId(shipmentId, shipmentSettingsOptional);
            }
        }
        return shipmentId;
    }

    private String getShipmentId(String shipmentId, Optional<ShipmentSettingsDetails> shipmentSettingsOptional) {
        if(StringUtility.isEmpty(shipmentId)) {
            if(shipmentSettingsOptional.isPresent()) {
                log.info("CR-ID {} || no common sequence found and shipment settings data is: {}",
                        LoggerHelper.getRequestIdFromMDC(),
                        jsonHelper.convertToJson(shipmentSettingsOptional.get()));
            }
            log.info("CR-ID {} || no common sequence found", LoggerHelper.getRequestIdFromMDC());
            shipmentId = Constants.SHIPMENT_ID_PREFIX + getShipmentsSerialNumber();
        }
        return shipmentId;
    }

    private String getCustomizedShipmentProcessNumber(ProductProcessTypes productProcessType, ShipmentDetails currentShipment) throws RunnerException {
        List<TenantProducts> tenantProducts = productEngine.populateEnabledTenantProducts();
        // to check the commmon sequence
        var sequenceNumber = productEngine.getCommonSequenceNumber(currentShipment.getTransportMode(), ProductProcessTypes.Consol_Shipment_TI);
        if (sequenceNumber != null && !sequenceNumber.isEmpty()) {
            return sequenceNumber;
        }
        var identifiedProduct = productEngine.identifyProduct(currentShipment, tenantProducts);
        if (identifiedProduct == null){
            return "";
        }
        var sequenceSettings = getNextNumberHelper.getProductSequence(identifiedProduct.getId(), productProcessType);
        if(sequenceSettings == null){
            sequenceSettings = productEngine.getShipmentProductWithOutContainerType(currentShipment, productProcessType, tenantProducts);
            if (sequenceSettings == null)
            {
                // get default product type for shipment
                var defaultProduct = productEngine.getDefaultShipmentProduct(tenantProducts);
                if (defaultProduct == null || identifiedProduct == defaultProduct) {
                    return "";
                }
                sequenceSettings = getNextNumberHelper.getProductSequence(defaultProduct.getId(), productProcessType);
                if (sequenceSettings == null) {
                    return "";
                }
            }
        }
        String prefix = sequenceSettings.getPrefix() == null ? "" : sequenceSettings.getPrefix();
        var user = UserContext.getUser();
        return getNextNumberHelper.generateCustomSequence(sequenceSettings, prefix, user.TenantId, true, null, false);
    }

    private String getShipmentsSerialNumber() {
        // Moving this responsibility to v1 sequnce table to avoid syncing overhead
        return v1Service.getShipmentSerialNumber();
    }

    public ResponseEntity<IRunnerResponse> syncShipmentAuditLogsToService(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            AuditLogsSyncRequest request = (AuditLogsSyncRequest) commonRequestModel.getData();
            if(request.getGuid() == null)
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            Optional<ShipmentDetails> oldEntity = shipmentDao.findByGuid(request.getGuid());
            if(oldEntity.isPresent())
                syncEntityConversionService.auditLogsV1ToV2(request.getChangeLogs(), oldEntity.get().getId());
            else
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e){
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> completeV1ShipmentCreateAndUpdate(CommonRequestModel commonRequestModel, Map<UUID, String> map, List<NotesRequest> customerBookingNotes, boolean dataMigration, List<AuditLogRequestV2> auditLogRequestV2, String createdBy) throws RunnerException {

        ShipmentRequest shipmentRequest = (ShipmentRequest) commonRequestModel.getData();

        List<BookingCarriageRequest> bookingCarriageRequestList = shipmentRequest.getBookingCarriagesList();
        List<TruckDriverDetailsRequest> truckDriverDetailsRequestList = shipmentRequest.getTruckDriverDetails();
        List<PackingRequest> packingRequestList = shipmentRequest.getPackingList();
        Set<ContainerRequest> containerRequestList = shipmentRequest.getContainersList();
        List<ELDetailsRequest> elDetailsRequestList = shipmentRequest.getElDetailsList();
        List<EventsRequest> eventsRequestList = shipmentRequest.getEventsList();
        List<NotesRequest> notesRequestList = shipmentRequest.getNotesList();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = shipmentRequest.getReferenceNumbersList();
        List<RoutingsRequest> routingsRequestList = shipmentRequest.getRoutingsList();
        List<ServiceDetailsRequest> serviceDetailsRequestList = shipmentRequest.getServicesList();
        List<PartiesRequest> shipmentAddressesRequestList = shipmentRequest.getShipmentAddresses();

        UUID guid = shipmentRequest.getGuid();
        Optional<ShipmentDetails> oldEntity = shipmentDao.findByGuid(guid);

        if (dataMigration) {
            MDC.put("skip-audit-log", "true");
        }

        Set<ConsolidationDetails> tempConsolidations = getTempConsolidations(shipmentRequest);

        try {
            List<Containers> oldContainers = null;
            Long id = null;
            boolean isCreate = true;
            if(oldEntity.isPresent()) {
                id = oldEntity.get().getId();
                oldContainers = getOldContainers(oldEntity, oldContainers);
                isCreate = false;
            }
            shipmentRequest.setConsolidationList(null);
            ShipmentDetails entity = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
            if (!tempConsolidations.isEmpty())
                entity.setConsolidationList(tempConsolidations);
            entity.setId(id);
            Set<Containers> updatedContainers = getUpdatedContainers(containerRequestList, tempConsolidations, entity, oldContainers, oldEntity);
            entity.setContainersList(updatedContainers);
            String operation = DBOperationType.CREATE.name();
            String oldEntityJsonString = null;

            if(entity.getSourceGuid() != null && entity.getGuid() != null && !Objects.equals(entity.getSourceGuid(), entity.getGuid())){
                entity.setEntityTransfer(true);
            }
            if(id == null) {
                entity = shipmentDao.save(entity, true);
                id = entity.getId();
            } else {
                operation = DBOperationType.UPDATE.name();
                oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
                entity = shipmentDao.update(entity, true);
            }

            shipmentDao.saveCreatedDateAndUser(id, createdBy, shipmentRequest.getCreatedAt());

            createAuditLog(entity, oldEntityJsonString, operation);
            if (dataMigration) {
                createV1AuditLogs(entity.getId(), auditLogRequestV2);
            }

            processListRequestTypes(map, bookingCarriageRequestList, entity, id, truckDriverDetailsRequestList, packingRequestList, tempConsolidations, updatedContainers, elDetailsRequestList, eventsRequestList, referenceNumbersRequestList, routingsRequestList, serviceDetailsRequestList, shipmentAddressesRequestList, notesRequestList);
            processCustomerBookingNotes(customerBookingNotes, entity, id);
            Set<Containers> oldConts = null;
            if(oldEntity.isPresent())
                oldConts = oldEntity.get().getContainersList();
            if(!dataMigration)
                dependentServiceHelper.pushShipmentDataToDependentService(entity, isCreate, false, oldConts);
            ShipmentDetailsResponse response = jsonHelper.convertValue(entity, ShipmentDetailsResponse.class);

            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new ValidationException(e.getMessage());
        }
    }

    private List<Containers> getOldContainers(Optional<ShipmentDetails> oldEntity, List<Containers> oldContainers) {
        Set<Containers> containers = new HashSet<>();
        if (oldEntity.isPresent()) {
            containers = oldEntity.get().getContainersList();
        }
        if(containers != null && !containers.isEmpty()) {
            if(oldContainers == null)
                oldContainers = new ArrayList<>();
            oldContainers.addAll(containers);
        }
        return oldContainers;
    }

    private Set<Containers> getUpdatedContainers(Set<ContainerRequest> containerRequestList, Set<ConsolidationDetails> tempConsolidations, ShipmentDetails entity, List<Containers> oldContainers, Optional<ShipmentDetails> oldEntity) throws RunnerException {
        Set<Containers> updatedContainers = null;
        if (containerRequestList != null) {
            containerRequestList.forEach(e -> e.setShipmentsList(null));
            if(!tempConsolidations.isEmpty() && !entity.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.CONSOLIDATION_ID, tempConsolidations.iterator().next().getId(), "=");
                Pair<Specification<Containers>, Pageable> containerPair = fetchData(listCommonRequest, Containers.class);
                Page<Containers> oldConsolContainers = containerDao.findAll(containerPair.getLeft(), containerPair.getRight());
                if(!oldConsolContainers.isEmpty()) {
                    if(oldContainers == null)
                        oldContainers = new ArrayList<>();
                    oldContainers.addAll(oldConsolContainers.getContent());
                }
            }
            updatedContainers = new HashSet<>(containerDao.updateEntityFromShipmentV1(jsonHelper.convertValueToList(containerRequestList.stream().toList(), Containers.class), oldContainers));
        } else if(!oldEntity.isEmpty()){
            updatedContainers = oldEntity.get().getContainersList();
        }
        return updatedContainers;
    }

    private Set<ConsolidationDetails> getTempConsolidations(ShipmentRequest shipmentRequest) {
        Set<ConsolidationDetails> tempConsolidations = new HashSet<>();

        Set<ConsolidationDetailsRequest> consolidationDetailsRequests = shipmentRequest.getConsolidationList();
        if(consolidationDetailsRequests != null && !consolidationDetailsRequests.isEmpty()) {
            for(ConsolidationDetailsRequest consolidation : consolidationDetailsRequests) {
                Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findByGuid(consolidation.getGuid());
                if(consolidationDetails.isPresent()) {
                    tempConsolidations.add(consolidationDetails.get());
                }
            }
        }
        return tempConsolidations;
    }

    private void processListRequestTypes(Map<UUID, String> map, List<BookingCarriageRequest> bookingCarriageRequestList, ShipmentDetails entity, Long id, List<TruckDriverDetailsRequest> truckDriverDetailsRequestList, List<PackingRequest> packingRequestList, Set<ConsolidationDetails> tempConsolidations, Set<Containers> updatedContainers, List<ELDetailsRequest> elDetailsRequestList, List<EventsRequest> eventsRequestList, List<ReferenceNumbersRequest> referenceNumbersRequestList, List<RoutingsRequest> routingsRequestList, List<ServiceDetailsRequest> serviceDetailsRequestList, List<PartiesRequest> shipmentAddressesRequestList, List<NotesRequest> notesRequestList) throws RunnerException {
        if (bookingCarriageRequestList != null) {
            ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
            Pair<Specification<BookingCarriage>, Pageable> bookingCarriagePair = fetchData(listCommonRequest, BookingCarriage.class);
            Page<BookingCarriage> oldBookingCarriages = bookingCarriageDao.findAll(bookingCarriagePair.getLeft(), bookingCarriagePair.getRight());
            List<BookingCarriage> updatedBookingCarriages = bookingCarriageDao.updateEntityFromShipment(jsonHelper.convertValueToList(bookingCarriageRequestList, BookingCarriage.class), id, oldBookingCarriages.stream().toList());
            entity.setBookingCarriagesList(updatedBookingCarriages);
        }
        if (truckDriverDetailsRequestList != null) {
            ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
            Pair<Specification<TruckDriverDetails>, Pageable> truckDriverDetailsPair = fetchData(listCommonRequest, TruckDriverDetails.class);
            Page<TruckDriverDetails> oldTruckDriverDetails = truckDriverDetailsDao.findAll(truckDriverDetailsPair.getLeft(), truckDriverDetailsPair.getRight());
            List<TruckDriverDetails> updatedTruckDriverDetails = truckDriverDetailsDao.updateEntityFromShipment(jsonHelper.convertValueToList(truckDriverDetailsRequestList, TruckDriverDetails.class), id, oldTruckDriverDetails.stream().toList());
            entity.setTruckDriverDetails(updatedTruckDriverDetails);
        }
        if (packingRequestList != null) {
            ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
            Pair<Specification<Packing>, Pageable> packingPair = fetchData(listCommonRequest, Packing.class);
            Page<Packing> oldPackings = packingDao.findAll(packingPair.getLeft(), packingPair.getRight());
            List<Packing> oldConsolPackings = new ArrayList<>();
            if(entity.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && !tempConsolidations.isEmpty()) {
                listCommonRequest = constructListCommonRequest(Constants.CONSOLIDATION_ID, tempConsolidations.iterator().next().getId(), "=");
                packingPair = fetchData(listCommonRequest, Packing.class);
                oldConsolPackings = packingDao.findAll(packingPair.getLeft(), packingPair.getRight()).stream().toList();
            }
            List<Packing> updatedPackings = packingDao.updateEntityFromShipment(jsonHelper.convertValueToList(packingRequestList, Packing.class), id, oldPackings.stream().toList(), oldConsolPackings, updatedContainers, map);
            entity.setPackingList(updatedPackings);
        }
        if (elDetailsRequestList != null) {
            ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
            Pair<Specification<ELDetails>, Pageable> elDetailsPair = fetchData(listCommonRequest, ELDetails.class);
            Page<ELDetails> oldELDetails = elDetailsDao.findAll(elDetailsPair.getLeft(), elDetailsPair.getRight());
            List<ELDetails> updatedELDetails = elDetailsDao.updateEntityFromShipment(jsonHelper.convertValueToList(elDetailsRequestList, ELDetails.class), id, oldELDetails.stream().toList());
            entity.setElDetailsList(updatedELDetails);
        }
        if (eventsRequestList != null) {
            ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.SHIPMENT);
            Pair<Specification<Events>, Pageable> pair = fetchData(listCommonRequest, Events.class);
            Page<Events> oldEvents = eventDao.findAll(pair.getLeft(), pair.getRight());
            List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(jsonHelper.convertValueToList(eventsRequestList, Events.class), id, Constants.SHIPMENT, oldEvents.stream().toList());
            entity.setEventsList(updatedEvents);
        }
        if (referenceNumbersRequestList != null) {
            ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
            Pair<Specification<ReferenceNumbers>, Pageable> pair = fetchData(listCommonRequest, ReferenceNumbers.class);
            Page<ReferenceNumbers> oldReferenceNumbers = referenceNumbersDao.findAll(pair.getLeft(), pair.getRight());
            List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(jsonHelper.convertValueToList(referenceNumbersRequestList, ReferenceNumbers.class), id, oldReferenceNumbers.stream().toList());
            entity.setReferenceNumbersList(updatedReferenceNumbers);
        }
        if (routingsRequestList != null) {
            ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
            Pair<Specification<Routings>, Pageable> pair = fetchData(listCommonRequest, Routings.class);
            Page<Routings> oldRoutings = routingsDao.findAll(pair.getLeft(), pair.getRight());
            List<Routings> updatedRoutings = routingsDao.updateEntityFromShipment(jsonHelper.convertValueToList(routingsRequestList, Routings.class), id, oldRoutings.stream().toList());
            entity.setRoutingsList(updatedRoutings);
        }
        if (serviceDetailsRequestList != null) {
            ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
            Pair<Specification<ServiceDetails>, Pageable> pair = fetchData(listCommonRequest, ServiceDetails.class);
            Page<ServiceDetails> oldServiceDetails = serviceDetailsDao.findAll(pair.getLeft(), pair.getRight());
            List<ServiceDetails> updatedServiceDetails = serviceDetailsDao.updateEntityFromShipment(jsonHelper.convertValueToList(serviceDetailsRequestList, ServiceDetails.class), id, oldServiceDetails.stream().toList());
            entity.setServicesList(updatedServiceDetails);
        }
        if (shipmentAddressesRequestList != null) {
            ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.SHIPMENT_ADDRESSES);
            Pair<Specification<Parties>, Pageable> pair = fetchData(listCommonRequest, Parties.class);
            Page<Parties> oldParties = partiesDao.findAll(pair.getLeft(), pair.getRight());
            List<Parties> updatedParties = partiesDao.updateEntityFromOtherEntity(jsonHelper.convertValueToList(shipmentAddressesRequestList, Parties.class), id, Constants.SHIPMENT_ADDRESSES, oldParties.stream().toList());
            entity.setShipmentAddresses(updatedParties);
        }
        if (notesRequestList != null) {
            ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.SHIPMENT);
            Pair<Specification<Notes>, Pageable> pair = fetchData(listCommonRequest, Notes.class);
            Page<Notes> oldNoteList = notesDao.findAll(pair.getLeft(), pair.getRight());
            List<Notes> updatedNotes = notesDao.updateEntityFromOtherEntity(jsonHelper.convertValueToList(notesRequestList, Notes.class), id, Constants.SHIPMENT, oldNoteList.stream().toList());
            entity.setNotesList(updatedNotes);
        }
    }

    private void processCustomerBookingNotes(List<NotesRequest> customerBookingNotes, ShipmentDetails entity, Long id) {
        if (customerBookingNotes != null && !customerBookingNotes.isEmpty()) {
            ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.CUSTOMER_BOOKING);
            Pair<Specification<Notes>, Pageable> pair = fetchData(listCommonRequest, Notes.class);
            Page<Notes> oldNoteList = notesDao.findAll(pair.getLeft(), pair.getRight());
            if(oldNoteList == null || oldNoteList.isEmpty()) {
                notesDao.saveEntityFromOtherEntity(jsonHelper.convertValueToList(customerBookingNotes, Notes.class), id, Constants.CUSTOMER_BOOKING);
            }
        }
    }

    private void createV1AuditLogs(Long id, List<AuditLogRequestV2> auditLogRequestV2) {
        try {
            syncEntityConversionService.auditLogsV1ToV2(auditLogRequestV2, id);
        } catch (Exception ignored) {
            log.error("Error while migrating audit logs for id: " + id);
        }
    }

    private void createAuditLog(ShipmentDetails entity, String oldEntityJsonString, String operation)
    {
        try {
            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(entity)
                            .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, ShipmentDetails.class) : null)
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(entity.getId())
                            .operation(operation).build()
            );
        }
        catch (Exception e) {
            log.error("Error creating audit service log", e);
        }
    }

    public ResponseEntity<IRunnerResponse> calculateContainerSummary(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        CalculateContainerSummaryRequest request = (CalculateContainerSummaryRequest) commonRequestModel.getData();
        try {
            Set<Containers> containers = new HashSet<>(jsonHelper.convertValueToList(request.getContainersList(), Containers.class));
            ContainerSummaryResponse response = containerService.calculateContainerSummary(new ArrayList<>(containers), request.getTransportMode(), request.getShipmentType());
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> calculatePackSummary(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        CalculatePackSummaryRequest request = (CalculatePackSummaryRequest) commonRequestModel.getData();
        try {
            List<Packing> packingList = jsonHelper.convertValueToList(request.getPackingList(), Packing.class);
            PackSummaryResponse response = packingService.calculatePackSummary(packingList, request.getTransportMode(), request.getShipmentType(), new ShipmentMeasurementDetailsDto());
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getAllMasterData(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            Long id = commonRequestModel.getId();
            Optional<ShipmentDetails> shipmentDetailsOptional = shipmentDao.findShipmentByIdWithQuery(id);
            if (!shipmentDetailsOptional.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_DETAILS_NULL_FOR_ID_ERROR, id);
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            ShipmentDetails shipmentDetails = shipmentDetailsOptional.get();
            long start = System.currentTimeMillis();
            List<String> includeColumns = FieldUtils.getMasterDataAnnotationFields(List.of(createFieldClassDto(ShipmentDetails.class, null), createFieldClassDto(AdditionalDetails.class, "additionalDetails.")));
            includeColumns.addAll(FieldUtils.getTenantIdAnnotationFields(List.of(createFieldClassDto(ShipmentDetails.class, null))));
            includeColumns.addAll(ShipmentConstants.LIST_INCLUDE_COLUMNS);
            ShipmentDetailsResponse shipmentDetailsResponse = (ShipmentDetailsResponse) commonUtils.setIncludedFieldsToResponse(shipmentDetails, includeColumns.stream().collect(Collectors.toSet()), new ShipmentDetailsResponse());
            log.info("Total time taken in setting shipment details response {}", (System.currentTimeMillis() - start));
            Map<String, Object> response = fetchAllMasterDataByKey(shipmentDetails, shipmentDetailsResponse);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }
    private FieldClassDto createFieldClassDto(Class<?> clazz, String parentref) {
        FieldClassDto fieldClassDto = new FieldClassDto();
        fieldClassDto.setClazz(clazz);
        fieldClassDto.setFieldRef(parentref);
        return fieldClassDto;
    }
    /**
     * * fetchAllMasterDataByKey to be used for direct API
     * @param shipmentDetails
     * @param shipmentDetailsResponse
     * @return
     */
    @Override
    public Map<String, Object> fetchAllMasterDataByKey(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        Map<String, Object> masterDataResponse = new HashMap<>();
        var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllMasterDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllUnlocationDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllCarrierDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var currencyFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllCurrencyDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllCommodityTypesInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllTenantDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var wareHouseDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllWarehouseDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var activityDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllActivityDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var salesAgentFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllSalesAgentInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var containerTypeFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllContainerTypesInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var vesselsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllVesselDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var dgSubstanceFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllDGSubstanceDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, currencyFuture, commodityTypesFuture, tenantDataFuture, wareHouseDataFuture, activityDataFuture, salesAgentFuture,
                containerTypeFuture, vesselsFuture, dgSubstanceFuture).join();

        return masterDataResponse;
    }

    public void createShipmentPayload(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        createShipmentPayload(shipmentDetails, shipmentDetailsResponse, true);
    }

    /**
     * * createShipmentPayload to be used while retrieving shipment
     * @param shipmentDetails
     * @param shipmentDetailsResponse
     */
    public void createShipmentPayload(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, boolean getMasterData) {
        try {
            double start = System.currentTimeMillis();
            if(getMasterData) {
                var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllMasterDataInSingleCall(shipmentDetailsResponse, null)), executorServiceMasterData);
                var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllUnlocationDataInSingleCall(shipmentDetailsResponse, null)), executorServiceMasterData);
                var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllCarrierDataInSingleCall(shipmentDetailsResponse, null)), executorServiceMasterData);
                var currencyFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllCurrencyDataInSingleCall(shipmentDetailsResponse, null)), executorServiceMasterData);
                var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllCommodityTypesInSingleCall(shipmentDetailsResponse, null)), executorServiceMasterData);
                var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllTenantDataInSingleCall(shipmentDetailsResponse, null)), executorServiceMasterData);
                var wareHouseDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllWarehouseDataInSingleCall(shipmentDetailsResponse, null)), executorServiceMasterData);
                var activityDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllActivityDataInSingleCall(shipmentDetailsResponse, null)), executorServiceMasterData);
                var salesAgentFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllSalesAgentInSingleCall(shipmentDetailsResponse, null)), executorServiceMasterData);
                var containerTypeFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllContainerTypesInSingleCall(shipmentDetailsResponse, null)), executorServiceMasterData);
                CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, currencyFuture, commodityTypesFuture, tenantDataFuture, wareHouseDataFuture,
                        activityDataFuture, salesAgentFuture,
                        containerTypeFuture).join();
                log.info("{} | createShipmentPayload Time taken for V1 Masterdata: {} ms", LoggerHelper.getRequestIdFromMDC(), (System.currentTimeMillis() - start));
            }
            double mid = System.currentTimeMillis();
            Map<Long, ContainerResponse> map = new HashMap<>();
            Set<ContainerResponse> containers = shipmentDetailsResponse.getContainersList();
            if(containers != null)
                map = containers.stream().collect(Collectors.toMap(ContainerResponse::getId, Function.identity()));
            masterDataHelper.setContainersPacksAutoUpdateData(shipmentDetailsResponse, map);
            masterDataHelper.setTruckDriverDetailsData(shipmentDetailsResponse, map);
            log.info("{} | createShipmentPayload Time taken for setContainersPacksAutoUpdateData & setTruckDriverDetailsData: {} ms", LoggerHelper.getRequestIdFromMDC(), (System.currentTimeMillis() - mid));
            mid = System.currentTimeMillis();
            if(!Objects.isNull(shipmentDetails)) {
                double start1 = System.currentTimeMillis();
                shipmentDetailsResponse.setPackSummary(packingService.calculatePackSummary(shipmentDetails.getPackingList(), shipmentDetails.getTransportMode(), shipmentDetails.getShipmentType(), new ShipmentMeasurementDetailsDto()));
                log.info("Time taken to calculate pack summary for event:{} | Time: {} ms. || RequestId: {},  pack size: {}", LoggerEvent.SHIPMENT_RETRIEVE_COMPLETE_MASTER_DATA, (System.currentTimeMillis() - start1) , LoggerHelper.getRequestIdFromMDC(), CollectionUtils.isEmpty(shipmentDetails.getPackingList()) ? 0 : shipmentDetails.getPackingList().size());
                start1 = System.currentTimeMillis();
                shipmentDetailsResponse.setContainerSummary(containerService.calculateContainerSummary(new ArrayList<>(shipmentDetails.getContainersList()), shipmentDetails.getTransportMode(), shipmentDetails.getShipmentType()));
                log.info("Time taken to calculate container summary for event:{} | Time: {} ms. || RequestId: {}, container size: {}", LoggerEvent.SHIPMENT_RETRIEVE_COMPLETE_MASTER_DATA, (System.currentTimeMillis() - start1) , LoggerHelper.getRequestIdFromMDC(), CollectionUtils.isEmpty(shipmentDetails.getContainersList()) ? 0 : shipmentDetails.getContainersList().size());
                log.info("{} | createShipmentPayload Time taken for setPackSummary & setContainerSummary: {} ms", LoggerHelper.getRequestIdFromMDC(), (System.currentTimeMillis() - mid));
                mid = System.currentTimeMillis();
            }
            // fetch NTE status
            this.fetchNTEstatusForReceivingBranch(shipmentDetailsResponse);
            log.info("{} | createShipmentPayload Time taken for setPackSummary & fetchNTEstatusForReceivingBranch: {} ms", LoggerHelper.getRequestIdFromMDC(), (System.currentTimeMillis() - mid));
            mid = System.currentTimeMillis();
            mid = getUpdatedMidAfterAwbStatusAndSummary(shipmentDetailsResponse, mid);
            setShipmentCount(shipmentDetails, shipmentDetailsResponse, mid);
            log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.SHIPMENT_RETRIEVE_COMPLETE_MASTER_DATA, (System.currentTimeMillis() - start) , LoggerHelper.getRequestIdFromMDC());
        }
        catch (Exception ex) {
            log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_RETRIEVE, ex.getLocalizedMessage());
        }

    }

    private void setShipmentCount(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, double mid) {
        if(!Objects.isNull(shipmentDetails)) {
            Set<ConsolidationDetails> consolidationList = shipmentDetails.getConsolidationList();
            if(!Objects.isNull(consolidationList) && !consolidationList.isEmpty()){
                List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consolidationList.iterator().next().getId());
                if(!Objects.isNull(consoleShipmentMappings) && !consoleShipmentMappings.isEmpty())
                    shipmentDetailsResponse.setShipmentCount((long) consoleShipmentMappings.size());
                else
                    shipmentDetailsResponse.setShipmentCount(0L);
            } else {
                shipmentDetailsResponse.setShipmentCount(0L);
            }
            log.info("{} | createShipmentPayload Time taken for setting Notification Count: {} ms", LoggerHelper.getRequestIdFromMDC(), (System.currentTimeMillis() - mid));
        } else {
            shipmentDetailsResponse.setShipmentCount(0L);
        }
    }

    private double getUpdatedMidAfterAwbStatusAndSummary(ShipmentDetailsResponse shipmentDetailsResponse, double mid) {
        try {
            if(shipmentDetailsResponse.getId() != null) {
                var awb = awbDao.findByShipmentId(shipmentDetailsResponse.getId());
                if (awb != null && !awb.isEmpty()) {
                    if (awb.get(0).getAirMessageStatus() != null)
                        shipmentDetailsResponse.setAwbStatus(awb.get(0).getAirMessageStatus());
                    else
                        shipmentDetailsResponse.setAwbStatus(AwbStatus.AWB_GENERATED);
                }
            }

            if(!Boolean.TRUE.equals(shipmentDetailsResponse.getAdditionalDetails().getIsSummaryUpdated()))
                shipmentDetailsResponse.getAdditionalDetails().setSummary(shipmentDetailsResponse.getContainerSummary().getSummary());
            log.info("{} | createShipmentPayload Time taken for setAwbStatus & getIsSummaryUpdated: {} ms", LoggerHelper.getRequestIdFromMDC(), (System.currentTimeMillis() - mid));
            mid = System.currentTimeMillis();
        } catch (Exception e) {
            log.error("Erorr occured while setAwbStatus & getIsSummaryUpdated : {}", e.getMessage());
        }
        return mid;
    }

    private void fetchNTEstatusForReceivingBranch(ShipmentDetailsResponse shipmentDetailsResponse) {
        Boolean isInterBranchShip = false;
        if(!setIsNullOrEmpty(shipmentDetailsResponse.getConsolidationList())) {
            isInterBranchShip = shipmentDetailsResponse.getConsolidationList().iterator().next().getInterBranchConsole();
        }
        if(Objects.equals(shipmentDetailsResponse.getTransportMode(), TRANSPORT_MODE_AIR) &&
                (Objects.equals(shipmentDetailsResponse.getJobType(), SHIPMENT_TYPE_DRT) || isInterBranchShip) && shipmentDetailsResponse.getReceivingBranch() != null) {
            String transferStatus = networkTransferDao.findStatusByEntityIdAndEntityTypeAndTenantId(shipmentDetailsResponse.getId(), SHIPMENT, shipmentDetailsResponse.getReceivingBranch().intValue());
            shipmentDetailsResponse.setTransferStatus(transferStatus);
        } else if (!setIsNullOrEmpty(shipmentDetailsResponse.getConsolidationList())){
            var console = shipmentDetailsResponse.getConsolidationList().iterator().next();
            if(console.getReceivingBranch() != null) {
                String transferStatus = networkTransferDao.findStatusByEntityIdAndEntityTypeAndTenantId(console.getId(), CONSOLIDATION, console.getReceivingBranch().intValue());
                shipmentDetailsResponse.setTransferStatus(transferStatus);
            }
        }
    }

    public ResponseEntity<IRunnerResponse> cloneShipment(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(ShipmentConstants.SHIPMENT_RETRIEVE_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error(ShipmentConstants.SHIPMENT_RETRIEVE_NULL_REQUEST, LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
            if (!shipmentDetails.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            checkPermissionsForCloning(shipmentDetails.get());
            ShipmentRequest cloneShipmentDetails = jsonHelper.convertValue(shipmentDetails.get(), ShipmentRequest.class);
            cloneShipmentDetails.setId(null);
            cloneShipmentDetails.setGuid(null);
            cloneShipmentDetails.setHouseBill(null);
            cloneShipmentDetails.setBookingNumber(null);
            cloneShipmentDetails.setContainersList(null);
            cloneShipmentDetails.setRoutingsList(null);
            cloneShipmentDetails.setShipmentId(null);
            cloneShipmentDetails.setMasterBill(null);
            cloneShipmentDetails.setConsolidationList(null);
            cloneShipmentDetails.setStatus(ShipmentStatus.Created.getValue());
            cloneShipmentDetails.setConsolRef(null);
            cloneShipmentDetails.setEventsList(null);
            cloneShipmentDetails.setBookingReference(null);
            cloneShipmentDetails.setSourceGuid(null);
            cloneShipmentDetails.setClonedGuid(shipmentDetails.get().getGuid());
            cloneShipmentDetails.setContractId(null);
            cloneShipmentDetails.getAdditionalDetails().setDraftPrinted(null);
            cloneShipmentDetails.getAdditionalDetails().setPrintedOriginal(null);
            cloneShipmentDetails.getAdditionalDetails().setSurrenderPrinted(null);
            cloneShipmentDetails.getAdditionalDetails().setPickupDate(null);
            cloneShipmentDetails.getAdditionalDetails().setCargoDeliveredDate(null);
            cloneShipmentDetails.getAdditionalDetails().setCustomReleaseDate(null);
            cloneShipmentDetails.getAdditionalDetails().setDocTurnedOverToCustomer(null);
            cloneShipmentDetails.getAdditionalDetails().setProofOfDeliveryDate(null);
            cloneShipmentDetails.getAdditionalDetails().setWarehouseCargoArrivalDate(null);
            cloneShipmentDetails.getAdditionalDetails().setPickupByConsigneeCompleted(null);
            cloneShipmentDetails.getAdditionalDetails().setEmptyContainerReturned(null);
            cloneShipmentDetails.getAdditionalDetails().setSecurityStatusReceivedFrom(null);
            cloneShipmentDetails.getAdditionalDetails().setAdditionalSecurityInformation(null);
            cloneShipmentDetails.getAdditionalDetails().setRegulatedEntityCategory(null);
            cloneShipmentDetails.getAdditionalDetails().setIsExportCustomClearanceCompleted(null);
            cloneShipmentDetails.getAdditionalDetails().setBlInstructionReceived(null);
            cloneShipmentDetails.getAdditionalDetails().setCargoOutForDelivery(null);
            cloneShipmentDetails.getAdditionalDetails().setFcrNumber(0);
            cloneShipmentDetails.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));
            cloneShipmentDetails.setAutoUpdateWtVol(false);
            cloneShipmentDetails.setFileStatus(null);
            cloneShipmentDetails.setOceanDGStatus(null);
            cloneShipmentDetails.setCreatedBy(null);
            cloneShipmentDetails.setShipmentOrders(null);
            cloneShipmentDetails.setIsReceivingBranchAdded(null);
            cloneShipmentDetails.setIsTransferredToReceivingBranch(null);
            cloneShipmentDetails.setIsNetworkFile(null);
            cloneShipmentDetails.setIsReceivingBranchManually(null);
            cloneShipmentDetails.setReceivingBranch(null);
            cloneShipmentDetails.setTriangulationPartnerList(null);
            cloneShipmentDetails.setTriangulationPartner(null);
            if(!Objects.isNull(cloneShipmentDetails.getPackingList()))
                cloneShipmentDetails.getPackingList().forEach(e -> e.setId(null));

            cloneShipmentDetails.setShipmentCreatedOn(LocalDateTime.now());

            if(Constants.TRANSPORT_MODE_SEA.equals(cloneShipmentDetails.getTransportMode()) && Constants.DIRECTION_EXP.equals(cloneShipmentDetails.getDirection()) && !Constants.SHIPMENT_TYPE_DRT.equals(cloneShipmentDetails.getJobType()))
                cloneShipmentDetails.setHouseBill(generateCustomHouseBL(null));

            log.info("Shipment details cloning started for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            ShipmentDetailsResponse response = jsonHelper.convertValue(cloneShipmentDetails, ShipmentDetailsResponse.class);
            masterDataHelper.addAllUnlocationDataInSingleCall(response, null);
            masterDataHelper.addAllTenantDataInSingleCall(response, null);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> transportInstructionList(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            TIListRequest tiListRequest = (TIListRequest) commonRequestModel.getData();
            if(tiListRequest == null) {
                log.error("Request is empty for TI List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if(tiListRequest.getShipmentGuid() == null) {
                log.error("Shipment Guid is null for TI List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            V1DataResponse v1DataResponse = v1Service.fetchTransportInstructionList(tiListRequest);
            List<TIResponse> tiResponseList = jsonHelper.convertValueToList(v1DataResponse.entities, TIResponse.class);
            return ResponseHelper.buildSuccessResponse(tiResponseList);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> containerListForTI(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            TIContainerListRequest tiContainerListRequest = (TIContainerListRequest) commonRequestModel.getData();
            if(tiContainerListRequest == null) {
                log.error("Request is empty for container TI List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if(tiContainerListRequest.getShipmentGuid() == null) {
                log.error("Shipment Guid is null for conatiner TI List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            V1DataResponse v1DataResponse = v1Service.fetchContainersListForTI(tiContainerListRequest);
            List<TIContainerResponse> containerResponseList = jsonHelper.convertValueToList(v1DataResponse.entities, TIContainerResponse.class);
            return ResponseHelper.buildSuccessResponse(containerResponseList);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveByOrderId(String orderId) throws RunnerException {
        try {
            ShipmentDetailsResponse response = jsonHelper.convertValue(orderManagementAdapter.getOrder(orderId), ShipmentDetailsResponse.class);
            this.createShipmentPayload(null, response);
            masterDataHelper.addAllMasterDataInSingleCall(response, null);
            masterDataHelper.addAllUnlocationDataInSingleCall(response, null);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e){
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> generateCustomHouseBLNumber() throws RunnerException {
        try {
            return ResponseHelper.buildSuccessResponse(GenerateCustomHblResponse.builder().hblNumber(generateCustomHouseBL(null)).build());
        } catch (Exception e) {
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getShipmentFromConsol(Long consolidationId, String bookingNumber) {
        var tenantSettings = commonUtils.getShipmentSettingFromContext();
        // Populate shipment details on basis of tenant settings

        ShipmentDetailsResponse shipment;
        var consolidationResponse = consolidationDetailsDao.findById(consolidationId);

        if (consolidationResponse.isEmpty())
            throw new DataRetrievalFailureException("Failed to fetch the consolidation with id " + consolidationId);

        var consolidation = modelMapper.map(consolidationResponse.get(), ConsolidationDetailsResponse.class);
        String containerCategory = getContainerCategory(consolidation, consolidationResponse.get());

        shipment = buildShipmentDetailsRespomse(bookingNumber, consolidation, tenantSettings, containerCategory);

        shipment.setDepartment(commonUtils.getAutoPopulateDepartment(
                shipment.getTransportMode(), shipment.getDirection(), MdmConstants.SHIPMENT_MODULE
        ));

        setNotifyPartyInAdditionalDetails(consolidation, shipment);

        if(consolidation.getRoutingsList() != null && !consolidation.getRoutingsList().isEmpty() && Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableRouteMaster())) {
            List<RoutingsResponse> routingsResponse = consolidation.getRoutingsList().stream().
                    map(item -> {
                        RoutingsResponse newItem = modelMapper.map(item, RoutingsResponse.class);
                        newItem.setId(null);
                        newItem.setGuid(null);
                        newItem.setConsolidationId(null);
                        newItem.setBookingId(null);
                        return newItem;
                    }).
                    toList();
            shipment.setRoutingsList(routingsResponse);
        }

        processCarrierDetailsOrigin(shipment);
        if(shipment.getCarrierDetails().getEta() != null) {
            if(shipment.getAdditionalDetails().getDateOfIssue() == null)
                shipment.getAdditionalDetails().setDateOfIssue(shipment.getCarrierDetails().getEta());
            if(shipment.getAdditionalDetails().getDateOfReceipt() == null)
                shipment.getAdditionalDetails().setDateOfReceipt(shipment.getCarrierDetails().getEta());
        }

        setPartiesBroker(consolidation, shipment);

        //Generate HBL
        if(Constants.TRANSPORT_MODE_SEA.equals(shipment.getTransportMode()) && Constants.DIRECTION_EXP.equals(shipment.getDirection()))
            shipment.setHouseBill(generateCustomHouseBL(null));

        try {
            log.info("Fetching Tenant Model");
            TenantModel tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
            String currencyCode = tenantModel.currencyCode;
            shipment.setFreightLocalCurrency(currencyCode);
        } catch (Exception e){
            log.error("Failed in fetching tenant data from V1 with error : {}", e);
        }

        createShipmentPayload(modelMapper.map(shipment, ShipmentDetails.class), shipment);

        return ResponseHelper.buildSuccessResponse(shipment);
    }

    private void setPartiesBroker(ConsolidationDetailsResponse consolidation, ShipmentDetailsResponse shipment) {
        PartiesResponse parties;
        if(consolidation.getReceivingAgent() != null) {
            parties = jsonHelper.convertValue(consolidation.getReceivingAgent(), PartiesResponse.class);
            parties.setId(null);
            parties.setGuid(null);
            shipment.getAdditionalDetails().setImportBroker(parties);
        }
        if(consolidation.getSendingAgent() != null) {
            parties = jsonHelper.convertValue(consolidation.getSendingAgent(), PartiesResponse.class);
            parties.setId(null);
            parties.setGuid(null);
            shipment.getAdditionalDetails().setExportBroker(parties);
        }
        if(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getIsEntityTransferPrerequisiteEnabled())) {
            if(!commonUtils.checkIfPartyExists(shipment.getAdditionalDetails().getImportBroker())) {
                shipment.getAdditionalDetails().setImportBrokerCountry(commonUtils.getCountryFromUnLocCode(consolidation.getCarrierDetails().getDestinationLocCode()));
            }
            if(!commonUtils.checkIfPartyExists(shipment.getAdditionalDetails().getExportBroker())) {
                shipment.getAdditionalDetails().setExportBrokerCountry(commonUtils.getCountryFromUnLocCode(consolidation.getCarrierDetails().getOriginLocCode()));
            }
        }
    }

    private void processCarrierDetailsOrigin(ShipmentDetailsResponse shipment) {
        if(!isStringNullOrEmpty(shipment.getCarrierDetails().getOrigin())) {
            if(isStringNullOrEmpty(shipment.getAdditionalDetails().getPaidPlace()))
                shipment.getAdditionalDetails().setPaidPlace(shipment.getCarrierDetails().getOrigin());
            if(isStringNullOrEmpty(shipment.getAdditionalDetails().getPlaceOfIssue()))
                shipment.getAdditionalDetails().setPlaceOfIssue(shipment.getCarrierDetails().getOrigin());
            if(isStringNullOrEmpty(shipment.getAdditionalDetails().getPlaceOfSupply()))
                shipment.getAdditionalDetails().setPlaceOfSupply(shipment.getCarrierDetails().getOrigin());
        }
    }

    private void setNotifyPartyInAdditionalDetails(ConsolidationDetailsResponse consolidation, ShipmentDetailsResponse shipment) {
        if (consolidation.getConsolidationAddresses() != null) {
            consolidation.getConsolidationAddresses().stream().forEach(party -> {
                if (party.getType().equals("NP1")) {
                    shipment.getAdditionalDetails().setNotifyParty(
                            PartiesResponse.builder()
                                    .orgCode(party.getOrgCode())
                                    .addressCode(party.getAddressCode())
                                    .build());
                }
            });
        }
    }

    private ShipmentDetailsResponse buildShipmentDetailsRespomse(String bookingNumber, ConsolidationDetailsResponse consolidation, ShipmentSettingsDetails tenantSettings, String containerCategory) {
        var consolAllocation = consolidation.getAllocations();
        var consolCarrier = consolidation.getCarrierDetails();
        List<TriangulationPartnerResponse> triangulationPartnerResponseList = consolidation.getTriangulationPartnerList() != null ?
                consolidation.getTriangulationPartnerList().stream()
                        .filter(Objects::nonNull)
                        .map(tp -> TriangulationPartnerResponse.builder()
                                .triangulationPartner(tp.getTriangulationPartner())
                                .isAccepted(tp.getIsAccepted())
                                .build())
                        .toList() : null;
        return ShipmentDetailsResponse.builder()
                .transportMode(consolidation.getTransportMode() == null ? tenantSettings.getDefaultTransportMode() : consolidation.getTransportMode())
                .bookingNumber(bookingNumber)
                .consolidationList(Set.of(modelMapper.map(consolidation, ConsolidationListResponse.class)))
                .direction(consolidation.getShipmentType() == null ? tenantSettings.getDefaultShipmentType() : consolidation.getShipmentType())
                .jobType(Constants.SHIPMENT_TYPE_STD)
                .shipmentType(containerCategory == null ? tenantSettings.getDefaultContainerType() : containerCategory)
                .additionalDetails(AdditionalDetailResponse.builder()
                        .SMTPIGMDate(consolidation.getSmtpigmDate())
                        .SMTPIGMNumber(consolidation.getSmtpigmNumber())
                        .inwardDateAndTime(consolidation.getInwardDateAndTime())
                        .releaseType(consolidation.getReleaseType())
                        .warehouseId(consolidation.getWarehouseId())
                        .isInland(consolidation.getIsInland())
                        .original(consolidation.getOriginal())
                        .IGMFileNo(consolidation.getIgmFileNo())
                        .IGMFileDate(consolidation.getIgmFileDate())
                        .IGMInwardDate(consolidation.getIgmInwardDate())
                        .copy(consolidation.getCopy())
                        .customDeclType(consolidation.getDeclarationType())
                        .importBroker(consolidation.getReceivingAgent())
                        .exportBroker(consolidation.getSendingAgent())
                        .build())
                .carrierDetails(getCarrierDetailResponse(consolidation, consolCarrier))
                .weight(consolAllocation != null ? consolAllocation.getWeight() : null)
                .weightUnit(consolAllocation != null ? consolAllocation.getWeightUnit() : tenantSettings.getWeightChargeableUnit())
                .volume(consolAllocation != null ? consolAllocation.getVolume() : null)
                .volumeUnit(consolAllocation != null ? consolAllocation.getVolumeUnit() : tenantSettings.getVolumeChargeableUnit())
                .chargable(consolAllocation != null ? consolAllocation.getChargable() : null)
                .chargeableUnit(consolAllocation != null ? consolAllocation.getChargeableUnit() : null)
                .paymentTerms(getPaymentTerms(consolidation))
                .masterBill(getMasterBill(consolidation))
                .status(0)
                .source(Constants.SYSTEM)
                .createdBy(UserContext.getUser().getUsername())
                .customerCategory(CustomerCategoryRates.CATEGORY_5)
                .shipmentCreatedOn(LocalDateTime.now())
                .consolRef(consolidation.getConsolidationNumber())
                .receivingBranch(consolidation.getReceivingBranch())
                .documentationPartner(consolidation.getDocumentationPartner())
                .triangulationPartnerList(triangulationPartnerResponseList)
                .triangulationPartner(consolidation.getTriangulationPartner())
                .build();
    }

    private CarrierDetailResponse getCarrierDetailResponse(ConsolidationDetailsResponse consolidation, CarrierDetailResponse consolCarrier) {
        var origin = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getOrigin() : null;
        var destination = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getDestination() : null;
        var originPort = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getOriginPort() : null;
        var destinationPort = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getDestinationPort() : null;
        var voyage = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getVoyage() : null;
        var vessel = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getVessel() : null;
        var aircrafType = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getAircraftType() : null;
        var eta = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getEta() : null;
        var etd = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getEtd() : null;
        var ata = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getAta() : null;
        var atd = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getAtd() : null;
        return CarrierDetailResponse.builder()
                .ata(ata)
                .eta(eta)
                .atd(atd)
                .etd(etd)
                .origin(origin)
                .vessel(vessel)
                .originPort(originPort)
                .destinationPort(destinationPort)
                .shippingLine(consolCarrier != null ? consolCarrier.getShippingLine() : null)
                .voyage(voyage)
                .aircraftType(aircrafType)
                .destination(destination)
                .flightNumber(consolCarrier != null ? consolCarrier.getFlightNumber() : null)
                .cfs(consolidation.getCarrierDetails().getCfs())
                .build();
    }

    private String getMasterBill(ConsolidationDetailsResponse consolidation) {
        return consolidation.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) ? consolidation.getMawb() : consolidation.getBol();
    }

    private String getPaymentTerms(ConsolidationDetailsResponse consolidation) {
        return consolidation.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && consolidation.getShipmentType().equals("EXP")
                ? consolidation.getPayment() : null;
    }

    private String getContainerCategory(ConsolidationDetailsResponse consolidation, ConsolidationDetails consolidationResponse) {
        String containerCategory = consolidation.getContainerCategory();
        if(Objects.equals(consolidation.getTransportMode(), Constants.TRANSPORT_MODE_SEA) && consolidationResponse.getShipmentsList() != null && !consolidationResponse.getShipmentsList().isEmpty()){
            boolean isFcl = true;
            boolean isLcl = true;
            for (var ship: consolidationResponse.getShipmentsList()){
                if(!Objects.equals(ship.getShipmentType(), Constants.CARGO_TYPE_FCL))
                    isFcl = false;
                if(!Objects.equals(ship.getShipmentType(), Constants.SHIPMENT_TYPE_LCL))
                    isLcl = false;
            }
            if(isFcl)
                containerCategory = Constants.CARGO_TYPE_FCL;
            else if (isLcl)
                containerCategory = Constants.SHIPMENT_TYPE_LCL;
        }
        return containerCategory;
    }

    public String generateCustomHouseBL(ShipmentDetails shipmentDetails) {
        String res = null;
        if(shipmentDetails != null) {
            res = shipmentDetails.getHouseBill();
        }
        ShipmentSettingsDetails tenantSetting = commonUtils.getShipmentSettingFromContext();
        if(shipmentDetails == null && tenantSetting != null && Boolean.TRUE.equals(tenantSetting.getRestrictHblGen())) {
            return null;
        }

        if (shipmentDetails != null && tenantSetting.getCustomisedSequence()) {
            try {
                res = productEngine.getCustomizedBLNumber(shipmentDetails);
            } catch (Exception e) {
                log.error(e.getMessage());
            }
        }

        if(res == null || res.isEmpty()) {
            res = tenantSetting.getHousebillPrefix() ==  null ? "" : tenantSetting.getHousebillPrefix();
            String numberGeneration = tenantSetting.getHousebillNumberGeneration() ==  null ? "" : tenantSetting.getHousebillNumberGeneration();
            switch(numberGeneration) {
                case "Random" :
                    res += StringUtility.getRandomString(10);
                    break;
                case "Serial" :
                    String serialNumber = getShipmentsSerialNumber();
                    res += serialNumber;
                    break;
                default : res = "";
                    break;
            }
        }

        return res;
    }

    @Override
    public ResponseEntity<IRunnerResponse> getDefaultShipment() {
        String responseMsg;
        try {
            var tenantSettings = commonUtils.getShipmentSettingFromContext();
            // Populate shipment details on basis of tenant settings
            ShipmentDetailsResponse response = new ShipmentDetailsResponse();
            response.setAdditionalDetails(new AdditionalDetailResponse());
            response.setCarrierDetails(new CarrierDetailResponse());
            response.setTransportMode(tenantSettings.getDefaultTransportMode());
            response.setDirection(tenantSettings.getDefaultShipmentType());
            response.setShipmentType(tenantSettings.getDefaultContainerType());

            response.setVolumeUnit(tenantSettings.getVolumeChargeableUnit());
            response.setWeightUnit(tenantSettings.getWeightChargeableUnit());
            response.setStatus(0);
            response.setSource(Constants.SYSTEM);
            response.setCreatedBy(UserContext.getUser().getUsername());
            response.setCustomerCategory(CustomerCategoryRates.CATEGORY_5);
            response.setShipmentCreatedOn(LocalDateTime.now());
            response.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));
            response.setAutoUpdateWtVol(true);
            response.setDateType(ESTIMATED);
            //Generate HBL
            if(Constants.TRANSPORT_MODE_SEA.equals(response.getTransportMode()) && Constants.DIRECTION_EXP.equals(response.getDirection()))
                response.setHouseBill(generateCustomHouseBL(null));

            // Populate default department
            response.setDepartment(commonUtils.getAutoPopulateDepartment(
                    response.getTransportMode(), response.getDirection(), MdmConstants.SHIPMENT_MODULE
            ));

            setTenantAndDefaultAgent(response);

            if(Constants.TRANSPORT_MODE_SEA.equals(response.getTransportMode()) && Constants.DIRECTION_EXP.equals(response.getDirection()))
                response.setHouseBill(generateCustomHouseBL(null));

            this.createShipmentPayload(null, response);

            return ResponseHelper.buildSuccessResponse(response);
        } catch(Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private void setTenantAndDefaultAgent(ShipmentDetailsResponse response) {
        try {
            log.info("Fetching Tenant Model");
            TenantModel tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
            String currencyCode = tenantModel.currencyCode;
            response.setFreightLocalCurrency(currencyCode);
            List<UnlocationsResponse> unlocationsResponse = masterDataUtils.fetchUnlocationByOneIdentifier(EntityTransferConstants.ID, StringUtility.convertToString(tenantModel.getUnloco()));
            if (!Objects.isNull(unlocationsResponse) && !unlocationsResponse.isEmpty()) {
                response.getAdditionalDetails().setPlaceOfIssue(unlocationsResponse.get(0).getLocationsReferenceGUID());
                response.getAdditionalDetails().setPaidPlace(unlocationsResponse.get(0).getLocationsReferenceGUID());
                response.getAdditionalDetails().setPlaceOfSupply(unlocationsResponse.get(0).getLocationsReferenceGUID());
            }
            PartiesResponse partiesResponse = v1ServiceUtil.getDefaultAgentOrg(tenantModel);
            if(Constants.DIRECTION_EXP.equals(response.getDirection())) {
                response.getAdditionalDetails().setExportBroker(partiesResponse);
            } else if(Constants.DIRECTION_IMP.equals(response.getDirection())) {
                response.getAdditionalDetails().setImportBroker(partiesResponse);
            }
        } catch (Exception e) {
            log.error("Failed in fetching tenant data from V1 with error : {}", e);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getMasterDataMappings() {
        String responseMsg;
        try {
            List<MasterDataDescriptionResponse> response;

            //Get current Tenant's setting
            Optional<ShipmentSettingsDetails> optional = shipmentSettingsDao.findByTenantId(TenantContext.getCurrentTenant());
            var tenantSetting = optional.get();
            // get all the master data based on field names of
            response = masterDataUtils.getMasterDataDescription(tenantSetting);

            return  ResponseHelper.buildSuccessResponse(response);
        } catch(Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> attachListShipment(CommonRequestModel commonRequestModel){
        AttachListShipmentRequest request = (AttachListShipmentRequest) commonRequestModel.getData();
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(request.getConsolidationId());
        if (!consolidationDetails.isPresent()) {
            log.debug("Consolidation Details is null for Id {} with Request Id {}", request.getConsolidationId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        request.setIncludeTbls(Arrays.asList(Constants.ADDITIONAL_DETAILS, Constants.CLIENT, Constants.CONSIGNER, Constants.CONSIGNEE, Constants.CARRIER_DETAILS, Constants.PICKUP_DETAILS, Constants.DELIVERY_DETAILS));
        ListCommonRequest listRequest = setCrieteriaForAttachShipment(request, consolidationDetails.get());
        Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(listRequest, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> spec = tuple.getLeft();
        if(shipmentSettingsDetails.getIsShipmentLevelContainer() != null && shipmentSettingsDetails.getIsShipmentLevelContainer())
            spec = spec.and(notInConsoleMappingTable());
        else
            spec = spec.and(notInConsoleMappingTable()).and(notInContainerMappingTable());
        if(Boolean.TRUE.equals(consolidationDetails.get().getInterBranchConsole()))
            commonUtils.setInterBranchContextForHub();
        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(spec , tuple.getRight());
        return ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoListForAttachListShipment(shipmentDetailsPage.getContent(), true),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements());
    }
    public static Specification<ShipmentDetails> notInConsoleMappingTable() {
        return (root, query, criteriaBuilder) -> criteriaBuilder.isEmpty(root.get(Constants.CONSOLIDATION_LIST));
    }
    public static Specification<ShipmentDetails> notInContainerMappingTable() {
        return (root, query, criteriaBuilder) -> criteriaBuilder.isEmpty(root.get(Constants.CONTAINERS_LIST));
    }

    private ListCommonRequest setCrieteriaForAttachShipment(AttachListShipmentRequest request, ConsolidationDetails consolidationDetails) {
        var tenantSettings = commonUtils.getCurrentTenantSettings();
        boolean setShipmentTypefilter = false;
        boolean isFcl = true;
        boolean isLcl = true;
        Set<ShipmentDetails> shipmentDetailsList = consolidationDetails.getShipmentsList();
        if(Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_SEA) && shipmentDetailsList != null && !shipmentDetailsList.isEmpty()){
            setShipmentTypefilter = true;
            for (var ship: shipmentDetailsList) {
                if(!Objects.equals(ship.getShipmentType(), Constants.CARGO_TYPE_FCL))
                    isFcl = false;
                if (!Objects.equals(ship.getShipmentType(), Constants.SHIPMENT_TYPE_LCL)) {
                    isLcl = false;
                }
            }
        }
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationIdAll(request.getConsolidationId());
        List<Long> excludeShipments = consoleShipmentMappings.stream().map(ConsoleShipmentMapping::getShipmentId).toList();

        if(request.getFilterCriteria() != null && request.getFilterCriteria().isEmpty()){
            request.setFilterCriteria(Arrays.asList(FilterCriteria.builder().innerFilter(new ArrayList<>()).build()));
        }
        ListCommonRequest defaultRequest;
        defaultRequest = CommonUtils.andCriteria(Constants.TRANSPORT_MODE, consolidationDetails.getTransportMode(), "=", request);
        if(excludeShipments != null && !excludeShipments.isEmpty())
            defaultRequest = CommonUtils.andCriteria("id", excludeShipments, "NOTIN", defaultRequest);

        addDirectionCriteria(consolidationDetails, defaultRequest);
        addShipmentTypeCriteria(consolidationDetails, setShipmentTypefilter, isFcl, defaultRequest, isLcl);
        CommonUtils.andCriteria(Constants.STATUS, 2, "!=", defaultRequest);
        CommonUtils.andCriteria(Constants.STATUS, 3, "!=", defaultRequest);
        if(checkForNonDGConsoleAndAirDgFlagAndNonDGUser(consolidationDetails))
            CommonUtils.andCriteria(CONTAINS_HAZARDOUS, false, "=", defaultRequest);
        List<FilterCriteria> criterias = defaultRequest.getFilterCriteria();
        List<FilterCriteria> innerFilters = criterias.get(0).getInnerFilter();
        Criteria criteria = Criteria.builder().fieldName(Constants.TRANSPORT_MODE).operator("!=").value(Constants.TRANSPORT_MODE_AIR).build();
        FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        List<FilterCriteria> innerFilers1 = new ArrayList<>();
        innerFilers1.add(filterCriteria);
        criteria = Criteria.builder().fieldName(Constants.JOB_TYPE).operator("!=").value(Constants.SHIPMENT_TYPE_DRT).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
        innerFilers1.add(filterCriteria);
        criteria = Criteria.builder().fieldName(Constants.JOB_TYPE).operator(Constants.IS_NULL).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
        innerFilers1.add(filterCriteria);
        filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
        innerFilters.add(filterCriteria);
        CarrierDetails consolidationCarrierDetails = consolidationDetails.getCarrierDetails();
        addCriteriaForAir(consolidationDetails, tenantSettings, consolidationCarrierDetails, defaultRequest);

        defaultRequest = processEtaMatchRequest(request, consolidationDetails, tenantSettings, consolidationCarrierDetails, defaultRequest, innerFilters);
        defaultRequest = processEtdMatchRequest(request, consolidationDetails, tenantSettings, consolidationCarrierDetails, defaultRequest, innerFilters);
        processScheduleMatchRequest(request, consolidationDetails, consolidationCarrierDetails, innerFilters);
        return defaultRequest;
    }

    private void addShipmentTypeCriteria(ConsolidationDetails consolidationDetails, boolean setShipmentTypefilter, boolean isFcl, ListCommonRequest defaultRequest, boolean isLcl) {
        if(setShipmentTypefilter){
            if(isFcl)
                CommonUtils.andCriteria(Constants.SHIPMENT_TYPE, Constants.CARGO_TYPE_FCL,"=" , defaultRequest);
            else if (isLcl)
                CommonUtils.andCriteria(Constants.SHIPMENT_TYPE,  Constants.SHIPMENT_TYPE_LCL,"=" , defaultRequest);
        }
        if(Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && !Objects.isNull(consolidationDetails.getContainerCategory())) {
            CommonUtils.andCriteria(Constants.SHIPMENT_TYPE, consolidationDetails.getContainerCategory(),"=" , defaultRequest);
        }
    }

    private void addDirectionCriteria(ConsolidationDetails consolidationDetails, ListCommonRequest defaultRequest) {
        if(!Objects.isNull(consolidationDetails.getShipmentType()))
            CommonUtils.andCriteria(Constants.DIRECTION, consolidationDetails.getShipmentType(), "=", defaultRequest);
        else
            CommonUtils.andCriteria(Constants.DIRECTION, "", Constants.IS_NULL, defaultRequest);
    }

    private void processScheduleMatchRequest(AttachListShipmentRequest request, ConsolidationDetails consolidationDetails, CarrierDetails consolidationCarrierDetails, List<FilterCriteria> innerFilters) {
        if(Boolean.TRUE.equals(request.getScheduleMatch())){
            if(Objects.equals(consolidationDetails.getTransportMode(),Constants.TRANSPORT_MODE_AIR)){
                processAirScheduledMatchRequest(consolidationCarrierDetails, innerFilters);
            }
            else if(Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_SEA)){
                processSeaScheduledMatchRequest(consolidationCarrierDetails, innerFilters);
            }
        }
    }

    private void processSeaScheduledMatchRequest(CarrierDetails consolidationCarrierDetails, List<FilterCriteria> innerFilters) {
        Criteria criteria;
        List<FilterCriteria> innerFilers1;
        FilterCriteria filterCriteria;
        innerFilers1 = new ArrayList<>();
        if(!Objects.isNull(consolidationCarrierDetails.getVessel()))
            criteria = Criteria.builder().fieldName(Constants.VESSEL).operator("=").value(consolidationCarrierDetails.getVessel()).build();
        else
            criteria = Criteria.builder().fieldName(Constants.VESSEL).operator(Constants.IS_NULL).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        innerFilers1.add(filterCriteria);
        criteria = Criteria.builder().fieldName(Constants.VESSEL).operator(Constants.IS_NULL).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
        innerFilers1.add(filterCriteria);
        filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
        innerFilters.add(filterCriteria);

        innerFilers1 = new ArrayList<>();
        if(!Objects.isNull(consolidationCarrierDetails.getVoyage()))
            criteria = Criteria.builder().fieldName(Constants.VOYAGE).operator("=").value(consolidationCarrierDetails.getVoyage()).build();
        else
            criteria = Criteria.builder().fieldName(Constants.VOYAGE).operator(Constants.IS_NULL).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        innerFilers1.add(filterCriteria);
        criteria = Criteria.builder().fieldName(Constants.VOYAGE).operator(Constants.IS_NULL).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
        innerFilers1.add(filterCriteria);
        filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
        innerFilters.add(filterCriteria);
    }

    private void processAirScheduledMatchRequest(CarrierDetails consolidationCarrierDetails, List<FilterCriteria> innerFilters) {
        List<FilterCriteria> innerFilers1;
        Criteria criteria;
        FilterCriteria filterCriteria;
        innerFilers1 = new ArrayList<>();
        if(!Objects.isNull(consolidationCarrierDetails.getFlightNumber()))
            criteria = Criteria.builder().fieldName(Constants.FLIGHT_NUMBER).operator("=").value(consolidationCarrierDetails.getFlightNumber()).build();
        else
            criteria = Criteria.builder().fieldName(Constants.FLIGHT_NUMBER).operator(Constants.IS_NULL).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        innerFilers1.add(filterCriteria);
        criteria = Criteria.builder().fieldName(Constants.FLIGHT_NUMBER).operator(Constants.IS_NULL).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
        innerFilers1.add(filterCriteria);
        filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
        innerFilters.add(filterCriteria);

        innerFilers1 = new ArrayList<>();
        if(!Objects.isNull(consolidationCarrierDetails.getShippingLine()))
            criteria = Criteria.builder().fieldName(Constants.SHIPPING_LINE).operator("=").value(consolidationCarrierDetails.getShippingLine()).build();
        else
            criteria = Criteria.builder().fieldName(Constants.SHIPPING_LINE).operator(Constants.IS_NULL).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        innerFilers1.add(filterCriteria);
        criteria = Criteria.builder().fieldName(Constants.SHIPPING_LINE).operator(Constants.IS_NULL).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
        innerFilers1.add(filterCriteria);
        filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
        innerFilters.add(filterCriteria);
    }

    private ListCommonRequest processEtdMatchRequest(AttachListShipmentRequest request, ConsolidationDetails consolidationDetails, V1TenantSettingsResponse tenantSettings, CarrierDetails consolidationCarrierDetails, ListCommonRequest defaultRequest, List<FilterCriteria> innerFilters) {
        FilterCriteria filterCriteria;
        List<FilterCriteria> innerFilers1;
        Criteria criteria;
        if(Boolean.TRUE.equals(request.getEtdMatch())){

            if(Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)
                    && Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
                if (!Objects.isNull(consolidationCarrierDetails.getEtd())) {
                    LocalDateTime etd = consolidationCarrierDetails.getEtd();
                    var thresholdETDFrom = etd.plusDays(-1);
                    var thresholdETDTo = etd.plusDays(1);
                    defaultRequest = CommonUtils.andCriteria("etd", thresholdETDFrom, ">=", defaultRequest);
                    defaultRequest = CommonUtils.andCriteria("etd", thresholdETDTo, "<=", defaultRequest);
                }
            }
            else {
                innerFilers1 = new ArrayList<>();
                if (!Objects.isNull(consolidationCarrierDetails.getEtd()))
                    criteria = Criteria.builder().fieldName("etd").operator("=").value(consolidationCarrierDetails.getEtd()).build();
                else
                    criteria = Criteria.builder().fieldName("etd").operator(Constants.IS_NULL).build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).build();
                innerFilers1.add(filterCriteria);
                criteria = Criteria.builder().fieldName("etd").operator(Constants.IS_NULL).build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
                innerFilers1.add(filterCriteria);
                filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
                innerFilters.add(filterCriteria);
            }
        }
        return defaultRequest;
    }

    private ListCommonRequest processEtaMatchRequest(AttachListShipmentRequest request, ConsolidationDetails consolidationDetails, V1TenantSettingsResponse tenantSettings, CarrierDetails consolidationCarrierDetails, ListCommonRequest defaultRequest, List<FilterCriteria> innerFilters) {
        List<FilterCriteria> innerFilers1;
        Criteria criteria;
        FilterCriteria filterCriteria;
        if(Boolean.TRUE.equals(request.getEtaMatch())){
            if(Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && Objects.equals(consolidationDetails.getShipmentType(), Constants.DIRECTION_EXP)
                && Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
                if (!Objects.isNull(consolidationCarrierDetails.getEta())) {
                    LocalDateTime eta = consolidationCarrierDetails.getEta();
                    var thresholdETAFrom = eta.plusDays(-1);
                    var thresholdETATo = eta.plusDays(1);

                    defaultRequest = CommonUtils.andCriteria("eta", thresholdETAFrom, ">=", defaultRequest);
                    defaultRequest = CommonUtils.andCriteria("eta", thresholdETATo, "<=", defaultRequest);
                }
            }
            else {
                innerFilers1 = new ArrayList<>();
                if (!Objects.isNull(consolidationCarrierDetails.getEta()))
                    criteria = Criteria.builder().fieldName("eta").operator("=").value(consolidationCarrierDetails.getEta()).build();
                else
                    criteria = Criteria.builder().fieldName("eta").operator(Constants.IS_NULL).build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).build();
                innerFilers1.add(filterCriteria);
                criteria = Criteria.builder().fieldName("eta").operator(Constants.IS_NULL).build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
                innerFilers1.add(filterCriteria);
                filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
                innerFilters.add(filterCriteria);
            }
        }
        return defaultRequest;
    }

    private void addCriteriaForAir(ConsolidationDetails consolidationDetails, V1TenantSettingsResponse tenantSettings, CarrierDetails consolidationCarrierDetails, ListCommonRequest defaultRequest) {
        if(!Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)
                || Boolean.FALSE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
            if (!Objects.isNull(consolidationCarrierDetails.getOriginPort()))
                CommonUtils.andCriteria(Constants.ORIGIN_PORT, consolidationCarrierDetails.getOriginPort(), "=", defaultRequest);
            else
                CommonUtils.andCriteria(Constants.ORIGIN_PORT, "", Constants.IS_NULL, defaultRequest);
            if (!Objects.isNull(consolidationCarrierDetails.getDestinationPort()))
                CommonUtils.andCriteria(Constants.DESTINATION_PORT, consolidationCarrierDetails.getDestinationPort(), "=", defaultRequest);
            else
                CommonUtils.andCriteria(Constants.DESTINATION_PORT, "", Constants.IS_NULL, defaultRequest);
        }
    }

    private boolean isAirDgUser() {
        return  LicenseContext.isDgAirLicense();
    }

    private boolean checkForAirDGFlag(ConsolidationDetails consolidationDetails) {
        if(!Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag()))
            return false;
        return Constants.TRANSPORT_MODE_AIR.equals(consolidationDetails.getTransportMode());
    }

    private boolean checkForNonDGConsoleAndAirDgFlagAndNonDGUser(ConsolidationDetails consolidationDetails) {
        if(!checkForAirDGFlag(consolidationDetails))
            return false;
        if(Boolean.TRUE.equals(consolidationDetails.getHazardous()))
            return false;
        return !isAirDgUser();
    }

    public boolean checkAttachDgAirShipments(ConsolidationDetails consolidationDetails){
        if(!Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR))
            return true;
        if(!Boolean.TRUE.equals(consolidationDetails.getHazardous()))
            return true;
        if(!Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag()))
            return true;
        if(consolidationDetails.getShipmentsList() == null || consolidationDetails.getShipmentsList().isEmpty())
            return false;
        return consolidationDetails.getShipmentsList().stream().anyMatch(ship -> Boolean.TRUE.equals(ship.getContainsHazardous()));
    }

    /**
     * back flows data of the current updated shipment to all its sibling shipments attached to the common console
     * @param current_shipment
     * @param old_shipment
     */
    @SuppressWarnings("java:S3655")
    private ConsolidationDetails updateLinkedShipmentData(ShipmentDetails shipment, ShipmentDetails oldEntity, ShipmentRequest shipmentRequest) throws RunnerException {
        Set<ConsolidationDetails> consolidationList = shipment.getConsolidationList();
        ConsolidationDetails consolidationDetails;
        V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        var linkedConsol = (consolidationList != null && !consolidationList.isEmpty()) ? consolidationList.iterator().next() : null;
        if(Boolean.TRUE.equals(tenantSettingsResponse.getEnableAirMessaging()) && linkedConsol != null && Objects.equals(shipment.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && Objects.equals(shipment.getAdditionalDetails().getEfreightStatus(), Constants.NON)) {
            consolidationDetails = consolidationDetailsDao.findById(linkedConsol.getId()).get();
            if (consolidationDetails != null && Objects.equals(consolidationDetails.getEfreightStatus(), Constants.EAW)) {
                throw new RunnerException("EFreight status can only be EAW as Consolidation EFrieght Status is EAW");
            }
        }
        processPackUtilisationCalculationInConsole(shipment, oldEntity, shipmentRequest, linkedConsol);
        boolean makeConsoleDG = checkForDGShipmentAndAirDgFlag(shipment) || checkForOceanDGShipment(shipment);
        AtomicBoolean makeConsoleNonDG = new AtomicBoolean(checkForNonDGShipmentAndAirDgFlag(shipment));
        AtomicBoolean makeConsoleSciT1 = new AtomicBoolean(shipment.getAdditionalDetails() != null && Objects.equals(shipment.getAdditionalDetails().getSci(), AwbConstants.T1));
        if(linkedConsol != null && isDiffPresentInOldNewConsole(shipment, oldEntity)) {
            consolidationDetails = processLinkedConsolidationDetails(shipment, oldEntity, consolidationList, makeConsoleDG, makeConsoleNonDG, makeConsoleSciT1);
            consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false, Boolean.TRUE.equals(shipment.getContainsHazardous()));
            return consolidationDetails;
        }
        else // only execute when above logic execution not required (i.e. saving all shipments not required)
            return changeConsolidationDGValues(makeConsoleDG, makeConsoleNonDG, consolidationList, shipment);
    }

    @SuppressWarnings("java:S3655")
    private ConsolidationDetails processLinkedConsolidationDetails(ShipmentDetails shipment, ShipmentDetails oldEntity, Set<ConsolidationDetails> consolidationList, boolean makeConsoleDG, AtomicBoolean makeConsoleNonDG, AtomicBoolean makeConsoleSciT1) throws RunnerException {
        ConsolidationDetails consolidationDetails;
        consolidationDetails = consolidationDetailsDao.findById(consolidationList.iterator().next().getId()).get();
        consolidationDetails.setBol(shipment.getMasterBill());
        if(consolidationDetails.getCarrierDetails() == null)
            consolidationDetails.setCarrierDetails(new CarrierDetails());
        consolidationDetails.getCarrierDetails().setAircraftType(shipment.getCarrierDetails().getAircraftType());
        consolidationDetails.getCarrierDetails().setShippingLine(shipment.getCarrierDetails().getShippingLine());
        consolidationDetails.getCarrierDetails().setVessel(shipment.getCarrierDetails().getVessel());
        consolidationDetails.getCarrierDetails().setVoyage(shipment.getCarrierDetails().getVoyage());
        consolidationDetails.setShipmentType(shipment.getDirection());

        if(makeConsoleDG)
            consolidationDetails.setHazardous(true);
        setSendindAndReceivingAgentForNonInterConsole(shipment, consolidationDetails);
        Boolean interBranchConsole = consolidationDetails.getInterBranchConsole();
        List<Long> shipmentIdList = getShipmentIdsExceptCurrentShipment(consolidationList.iterator().next().getId(), shipment);
        if (!shipmentIdList.isEmpty()) {
            processShipmentIdList(shipment, shipmentIdList, interBranchConsole, makeConsoleNonDG, makeConsoleSciT1);
        }
        if(makeConsoleNonDG.get())
            consolidationDetails.setHazardous(false);
        if(makeConsoleSciT1.get() && checkConsoleSciUpdateT1(shipment, oldEntity))
            consolidationDetails.setSci(AwbConstants.T1);
        else if(Objects.equals(consolidationDetails.getSci(), AwbConstants.T1) && !makeConsoleSciT1.get() && oldEntity != null && !Objects.equals(shipment.getAdditionalDetails().getSci(), oldEntity.getAdditionalDetails().getSci()))
            consolidationDetails.setSci(null);
        return consolidationDetails;
    }

    private boolean isDiffPresentInOldNewConsole(ShipmentDetails shipment, ShipmentDetails oldEntity) {
        return oldEntity == null || !Objects.equals(shipment.getMasterBill(), oldEntity.getMasterBill()) ||
                !Objects.equals(shipment.getDirection(), oldEntity.getDirection()) ||
                (shipment.getAdditionalDetails() != null && oldEntity.getAdditionalDetails() != null &&
                        (!Objects.equals(shipment.getAdditionalDetails().getSci(), oldEntity.getAdditionalDetails().getSci()) ||
                                !CommonUtils.checkSameParties(shipment.getAdditionalDetails().getExportBroker(), oldEntity.getAdditionalDetails().getExportBroker()) ||
                                !CommonUtils.checkSameParties(shipment.getAdditionalDetails().getImportBroker(), oldEntity.getAdditionalDetails().getImportBroker()))) ||
                (shipment.getCarrierDetails() != null && oldEntity.getCarrierDetails() != null &&
                        (!Objects.equals(shipment.getCarrierDetails().getVoyage(), oldEntity.getCarrierDetails().getVoyage()) ||
                                !Objects.equals(shipment.getCarrierDetails().getVessel(), oldEntity.getCarrierDetails().getVessel()) ||
                                !Objects.equals(shipment.getCarrierDetails().getShippingLine(), oldEntity.getCarrierDetails().getShippingLine()) ||
                                !Objects.equals(shipment.getCarrierDetails().getAircraftType(), oldEntity.getCarrierDetails().getAircraftType())
                        ));
    }

    private void setSendindAndReceivingAgentForNonInterConsole(ShipmentDetails shipment, ConsolidationDetails consolidationDetails) {
        if(!Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole())) {
            if (shipment.getAdditionalDetails() != null) {
                if(!CommonUtils.checkSameParties(shipment.getAdditionalDetails().getExportBroker(), consolidationDetails.getSendingAgent())) {
                    consolidationDetails.setSendingAgent(commonUtils.removeIdFromParty(shipment.getAdditionalDetails().getExportBroker()));
                }
                if(!CommonUtils.checkSameParties(shipment.getAdditionalDetails().getImportBroker(), consolidationDetails.getReceivingAgent())) {
                    consolidationDetails.setReceivingAgent(commonUtils.removeIdFromParty(shipment.getAdditionalDetails().getImportBroker()));
                }
            } else {
                consolidationDetails.setSendingAgent(null);
                consolidationDetails.setReceivingAgent(null);
            }
        }
    }

    private void processShipmentIdList(ShipmentDetails shipment, List<Long> shipmentIdList, Boolean interBranchConsole, AtomicBoolean makeConsoleNonDG, AtomicBoolean makeConsoleSciT1) throws RunnerException {
        List<ShipmentDetails> shipments = shipmentDao.findShipmentsByIds(shipmentIdList.stream().collect(
            Collectors.toSet()));
        var a = shipments.stream()
            .map(i -> {
                i.setMasterBill(shipment.getMasterBill());
                i.setDirection(shipment.getDirection());
                if (shipment.getCarrierDetails() != null) {
                    i.getCarrierDetails().setVoyage(shipment.getCarrierDetails().getVoyage());
                    i.getCarrierDetails().setVessel(shipment.getCarrierDetails().getVessel());
                    i.getCarrierDetails().setShippingLine(shipment.getCarrierDetails().getShippingLine());
                    i.getCarrierDetails().setAircraftType(shipment.getCarrierDetails().getAircraftType());
                }
                if(!Boolean.TRUE.equals(interBranchConsole)) {
                    if (shipment.getAdditionalDetails() != null && isExportOrImportBrokerPresent(shipment)) {
                        addAdditionalDetailsForShipment(shipment, i);
                    } else if(shipment.getAdditionalDetails() == null && i.getAdditionalDetails() != null) {
                        i.getAdditionalDetails().setExportBroker(null);
                        i.getAdditionalDetails().setImportBroker(null);
                    }
                }
                if (makeConsoleNonDG.get() && Boolean.TRUE.equals(i.getContainsHazardous()))
                    makeConsoleNonDG.set(false);
                if(Objects.equals(i.getAdditionalDetails().getSci(), AwbConstants.T1)){
                    makeConsoleSciT1.set(true);
                }
                return i;
            }).toList();
        shipmentDao.saveAll(a);
    }

    private boolean isExportOrImportBrokerPresent(ShipmentDetails shipment) {
        return CommonUtils.checkPartyNotNull(shipment.getAdditionalDetails().getExportBroker()) || CommonUtils.checkPartyNotNull(shipment.getAdditionalDetails().getImportBroker());
    }

    private void addAdditionalDetailsForShipment(ShipmentDetails shipment, ShipmentDetails i) {
        if(i.getAdditionalDetails() == null) {
            i.setAdditionalDetails(new AdditionalDetails());
        }
        if (!CommonUtils.checkSameParties(shipment.getAdditionalDetails().getExportBroker(), i.getAdditionalDetails().getExportBroker())) {
            i.getAdditionalDetails().setExportBroker(commonUtils.removeIdFromParty(shipment.getAdditionalDetails().getExportBroker()));
        }
        if (!CommonUtils.checkSameParties(shipment.getAdditionalDetails().getImportBroker(), i.getAdditionalDetails().getImportBroker())) {
            i.getAdditionalDetails().setImportBroker(commonUtils.removeIdFromParty(shipment.getAdditionalDetails().getImportBroker()));
        }
    }

    private void processPackUtilisationCalculationInConsole(ShipmentDetails shipment, ShipmentDetails oldEntity, ShipmentRequest shipmentRequest, ConsolidationDetails linkedConsol) {
        if(linkedConsol != null && shipmentRequest != null) {
            CalculatePackUtilizationRequest utilizationRequest = CalculatePackUtilizationRequest.builder()
                .consolidationId(linkedConsol.getId())
                .saveConsol(true)
                .shipmentRequest(shipmentRequest).build();
            packingService.savePackUtilisationCalculationInConsole(utilizationRequest);
        }
        else if(oldEntity != null && oldEntity.getConsolidationList() != null && !oldEntity.getConsolidationList().isEmpty()) {
            var oldConsolId = oldEntity.getConsolidationList().iterator().next().getId();
            CalculatePackUtilizationRequest utilizationRequest = CalculatePackUtilizationRequest.builder()
                    .consolidationId(oldConsolId)
                    .saveConsol(true)
                    .shipmentRequest(ShipmentRequest.builder().id(shipment.getId()).build()).build();
            packingService.savePackUtilisationCalculationInConsole(utilizationRequest);
        }
    }

    private List<Long> getShipmentIdsExceptCurrentShipment(Long consolidationId, ShipmentDetails shipment) {
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consolidationId);
        return consoleShipmentMappings.stream().filter(c -> !Objects.equals(c.getShipmentId(), shipment.getId()))
                .map(ConsoleShipmentMapping::getShipmentId).toList();
    }

    private ConsolidationDetails changeConsolidationDGValues(boolean makeConsoleDG, AtomicBoolean makeConsoleNonDG, Set<ConsolidationDetails> consolidationList, ShipmentDetails shipment) {
        if(consolidationList != null && !consolidationList.isEmpty()) {
            return changeConsolidationDGValues(makeConsoleDG, makeConsoleNonDG, consolidationList.iterator().next().getId(), shipment, null);
        }
        return null;
    }

    public ConsolidationDetails changeConsolidationDGValues(boolean makeConsoleDG, AtomicBoolean makeConsoleNonDG, Long consolidationId, ShipmentDetails shipment, ConsolidationDetails consolidationDetails) {
        if(makeConsoleDG) {
            consolidationDetails = getConsolidationDetails(consolidationId, consolidationDetails);
            return saveConsolidationDGValue(true, consolidationDetails);
        }
        if(makeConsoleNonDG.get()) {
            List<Long> shipmentIdList = getShipmentIdsExceptCurrentShipment(consolidationId, shipment);
            makeConsoleNonDG.set(checkIfAllShipmentsAreNonDG(shipmentIdList));
            if(makeConsoleNonDG.get()) {
                consolidationDetails = getConsolidationDetails(consolidationId, consolidationDetails);
                return saveConsolidationDGValue(false, consolidationDetails);
            }
        }
        return null;
    }

    public ConsolidationDetails getConsolidationDetails(Long consolidationId, ConsolidationDetails consolidationDetails) {
        if(!Objects.isNull(consolidationDetails))
            return consolidationDetails;
        Optional<ConsolidationDetails> optionalConsolidationDetails = consolidationDetailsDao.findById(consolidationId);
        if(optionalConsolidationDetails.isPresent())
            return optionalConsolidationDetails.get();
        throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
    }

    public boolean checkIfAllShipmentsAreNonDG(List<Long> shipmentIdList) {
        if (!shipmentIdList.isEmpty()) {
            List<ShipmentDetails> shipmentDetails = shipmentDao.findByShipmentIdInAndContainsHazardous(shipmentIdList, true);
            if(!CollectionUtils.isEmpty(shipmentDetails))
                return false;
        }
        return true;
    }

    public ConsolidationDetails saveConsolidationDGValue(boolean dgFlag, ConsolidationDetails consolidationDetails) {
        if( (!Boolean.TRUE.equals(consolidationDetails.getHazardous()) && dgFlag)
            || (!dgFlag && Boolean.TRUE.equals(consolidationDetails.getHazardous())) ) {
            consolidationDetails.setHazardous(dgFlag);
            consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false, dgFlag);
            return consolidationDetails;
        }
        return null;
    }

    private boolean checkForNonAirDGFlag(ShipmentDetails request, ShipmentSettingsDetails shipmentSettingsDetails) {
        if(!Constants.TRANSPORT_MODE_AIR.equals(request.getTransportMode()))
            return true;
        return !Boolean.TRUE.equals(shipmentSettingsDetails.getAirDGFlag());
    }

    private boolean checkForDGShipmentAndAirDgFlag(ShipmentDetails shipment) {
        if(checkForNonAirDGFlag(shipment, commonUtils.getShipmentSettingFromContext()))
            return false;
        return Boolean.TRUE.equals(shipment.getContainsHazardous());
    }

    private boolean checkForOceanDGShipment(ShipmentDetails shipmentDetails) {
        return TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode()) && Boolean.TRUE.equals(shipmentDetails.getContainsHazardous());
    }

    private boolean checkForNonDGShipmentAndAirDgFlag(ShipmentDetails shipment) {
        if(checkForNonAirDGFlag(shipment, commonUtils.getShipmentSettingFromContext()))
            return false;
        return !Boolean.TRUE.equals(shipment.getContainsHazardous());
    }

    private boolean checkConsoleSciUpdateT1(ShipmentDetails shipment, ShipmentDetails oldEntity) {
        if(shipment.getAdditionalDetails() == null) return false;
        if(Strings.isNullOrEmpty(shipment.getAdditionalDetails().getSci())) return false;
        if(!Objects.equals(shipment.getAdditionalDetails().getSci(), AwbConstants.T1)) return false;
        return oldEntity == null || !Objects.equals(shipment.getAdditionalDetails().getSci(),oldEntity.getAdditionalDetails().getSci());
    }

    @Override
    public ResponseEntity<IRunnerResponse> getIdFromGuid(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(ShipmentConstants.SHIPMENT_RETRIEVE_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getGuid() == null) {
                log.error("Request Guid Id is null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findByGuid(UUID.fromString(request.getGuid()));
            if (!shipmentDetails.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_DETAILS_NULL_FOR_GUID_ERROR, request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Shipment details fetched successfully for Guid {} with Request Id {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse(ShipmentDetailsResponse.builder().id(shipmentDetails.get().getId()).build());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getGuidFromId(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(ShipmentConstants.SHIPMENT_RETRIEVE_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error(ShipmentConstants.SHIPMENT_RETRIEVE_NULL_REQUEST, LoggerHelper.getRequestIdFromMDC());
            }
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(request.getId());
            if (!shipmentDetails.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_DETAILS_NULL_FOR_GUID_ERROR, request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Shipment details fetched successfully for Guid {} with Request Id {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse(ShipmentDetailsResponse.builder().guid(shipmentDetails.get().getGuid()).build());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @SuppressWarnings({"java:S1066", "java:S2583"})
    private void autoGenerateEvents(ShipmentDetails shipmentDetails) {
        Events response = null;
        if(shipmentDetails.getStatus() != null) {
            // LATER : remove this
            if(response != null) {
                if (shipmentDetails.getEventsList() == null)
                    shipmentDetails.setEventsList(new ArrayList<>());
                shipmentDetails.getEventsList().add(response);
            }
        }
    }

    private void autoGenerateCreateEvent(ShipmentDetails shipmentDetails) {
        Events response = null;
        response = createAutomatedEvents(shipmentDetails, EventConstants.SHCR, commonUtils.getUserZoneTime(LocalDateTime.now()), null);

        if (shipmentDetails.getEventsList() == null) {
            shipmentDetails.setEventsList(new ArrayList<>());
        }
        shipmentDetails.getEventsList().add(response);
    }

    private Events createAutomatedEvents(ShipmentDetails shipmentDetails, String eventCode,
            LocalDateTime actualDateTime, LocalDateTime estimatedDateTime) {
        Events events = initializeAutomatedEvents(shipmentDetails, eventCode, actualDateTime, estimatedDateTime);
        commonUtils.updateEventWithMasterData(List.of(events));
        // Persist the event
        eventDao.save(events);
        return events;
    }

    private Events initializeAutomatedEvents(ShipmentDetails shipmentDetails, String eventCode,
            LocalDateTime actualDateTime, LocalDateTime estimatedDateTime) {
        Events events = new Events();
        // Set event fields from shipment
        events.setActual(actualDateTime);
        events.setEstimated(estimatedDateTime);
        events.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);
        events.setIsPublicTrackingEvent(true);
        events.setEntityType(Constants.SHIPMENT);
        events.setEntityId(shipmentDetails.getId());
        events.setTenantId(TenantContext.getCurrentTenant());
        events.setEventCode(eventCode);
        events.setShipmentNumber(shipmentDetails.getShipmentId());
        events.setDirection(shipmentDetails.getDirection());
        // Attach to console as well
        eventDao.updateFieldsForShipmentGeneratedEvents(List.of(events), shipmentDetails);

        return events;
    }

    public ResponseEntity<IRunnerResponse> fetchShipmentsForConsoleId(CommonRequestModel commonRequestModel) throws RunnerException {
        CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
        if(request.getId() == null) {
            log.error("Request Id is null for Consolidation retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException("Id can't be null");
        }
        Long id = request.getId();
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(id);
        List<Long> shipmentIdsList = new ArrayList<>();
        if(consoleShipmentMappings != null && !consoleShipmentMappings.isEmpty()) {
            shipmentIdsList = consoleShipmentMappings.stream().map(x -> x.getShipmentId()).toList();
        }
        ListCommonRequest listCommonRequest = CommonUtils.andCriteria("id", shipmentIdsList, "IN", null);
        return fetchShipments(CommonRequestModel.buildRequest(listCommonRequest));
    }

    public ResponseEntity<IRunnerResponse> fetchActiveInvoices(CommonRequestModel commonRequestModel) throws RunnerException {
        CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
        if(request.getGuid() == null) {
            log.error("Request guid is null for fetch active invoices with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException("Shipment Guid can't be null");
        }

        boolean activeCharges = billingServiceAdapter.fetchActiveInvoices(request);
        CheckActiveInvoiceResponse checkActiveInvoiceResponse = CheckActiveInvoiceResponse.builder().IsAnyActiveInvoiceFound(activeCharges).build();

        /*
        activeCharges false means atleast one of the value is not 0
        return true because active charges are present
         */
        return ResponseHelper.buildSuccessResponse(checkActiveInvoiceResponse);
    }

    public ResponseEntity<IRunnerResponse> showAssignAllContainers(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ShipmentConsoleIdDto request = (ShipmentConsoleIdDto) commonRequestModel.getData();
            Long shipmentId = request.getShipmentId();
            Long consolidationId = request.getConsolidationId();
            List<ShipmentsContainersMapping> shipmentsContainersMappingList = shipmentsContainersMappingDao.findByShipmentId(shipmentId);
            List<Containers> containers = containerDao.findByConsolidationId(consolidationId);
            boolean showDialog = getShowDialog(shipmentsContainersMappingList, containers);
            int numberOfShipments = 0;
            List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consolidationId);
            if(consoleShipmentMappings != null && !consoleShipmentMappings.isEmpty())
                numberOfShipments = consoleShipmentMappings.size();
            AssignAllDialogDto response = new AssignAllDialogDto();
            response.setShowDialog(showDialog);
            response.setNumberOfShipments(numberOfShipments);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private boolean getShowDialog(List<ShipmentsContainersMapping> shipmentsContainersMappingList, List<Containers> containers) {
        boolean showDialog = false;
        if(shipmentsContainersMappingList != null && containers != null && !containers.isEmpty() &&
                containers.size() != shipmentsContainersMappingList.size())
        {
            ShipmentSettingsDetails shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant())).get(0);
            if(shipmentSettingsDetails.getMultipleShipmentEnabled() == null || !shipmentSettingsDetails.getMultipleShipmentEnabled()) {
                for (Containers containers1 : containers) {
                    if(containers1.getShipmentsList() == null || containers1.getShipmentsList().isEmpty()) {
                        showDialog = true;
                        break;
                    }
                }
            }
            else
                showDialog = true;
        }
        return showDialog;
    }

    public ResponseEntity<IRunnerResponse> fetchCreditLimit(String orgCode, String addressCode) throws RunnerException {
        if(StringUtility.isEmpty(orgCode)) {
            throw new RunnerException("OrgCode to fetch creditLimit can't be null");
        }
        AddressTranslationRequest.OrgAddressCode orgAddressCode = AddressTranslationRequest.OrgAddressCode.builder().OrgCode(orgCode).AddressCode(addressCode).build();
        try {
            V1DataResponse v1DataResponse = v1Service.fetchCreditLimit(orgAddressCode);
            if(v1DataResponse.entities == null) {
                log.debug(ShipmentConstants.NO_DATA_FOUND_FOR_ORG_CODE, orgCode);
                return ResponseHelper.buildSuccessResponse();
            }
            List<CreditLimitResponse> creditLimitResponses = jsonHelper.convertValueToList(v1DataResponse.getEntities(), CreditLimitResponse.class);
            if(creditLimitResponses == null || creditLimitResponses.isEmpty()) {
                log.debug(ShipmentConstants.NO_DATA_FOUND_FOR_ORG_CODE, orgCode);
                return ResponseHelper.buildSuccessResponse();
            }
            return ResponseHelper.buildDependentServiceResponse(creditLimitResponses.get(0), 0, 0);
        } catch (Exception e) {
            log.debug("No Data found for org code {} {}", orgCode, e.getMessage());
        }

        return ResponseHelper.buildSuccessResponse();
    }

    @Override
    public void updateDateAndStatus(Long id, LocalDateTime date, Integer status) throws RunnerException {
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
        if(shipmentDetails.isPresent()) {
            ShipmentDetails shipment = shipmentDetails.get();
            if(status != null && !Objects.equals(status, shipment.getStatus())) {
                shipment.setStatus(status);
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(id, SHIPMENT);
                Pair<Specification<Events>, Pageable> pair = fetchData(listCommonRequest, Events.class);
                shipment.setEventsList(new ArrayList<>(eventDao.findAll(pair.getLeft(), pair.getRight()).getContent().stream().toList()));
                shipmentDao.save(shipment, false);
                try {
                    shipmentSync.sync(shipment, null, null, shipment.getGuid().toString(), false);
                } catch (Exception e) {
                    log.error(SyncingConstants.ERROR_SYNCING_SHIPMENTS, e);
                }
            }
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> fetchEmails(Long shipmentId, Long consolidationId) {
        if(Objects.isNull(shipmentId) && Objects.isNull(consolidationId)) {
            log.error("Invalid request for fetchEmails");
            throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
        }
        if (!Objects.isNull(shipmentId)) {
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(shipmentId);
            if (shipmentDetails.isEmpty())
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            return v1ServiceUtil.fetchEmailIdsForShipment(shipmentDetails.get());
        }
        else if (!Objects.isNull(consolidationId)) {
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(consolidationId);
            if (consolidationDetails.isEmpty())
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            return v1ServiceUtil.fetchEmailIdsForConsolidation(consolidationDetails.get());
        }
        return ResponseHelper.buildFailedResponse(DaoConstants.DAO_INVALID_REQUEST_MSG);
    }

    @Override
    public ResponseEntity<IRunnerResponse> checkCreditLimitFromV1(CommonRequestModel commonRequestModel){
        CheckCreditLimitFromV1Request request = (CheckCreditLimitFromV1Request) commonRequestModel.getData();
        String checkCreditLimitDocs = IReport.checkCreditLimitDocs(request.getDocType());
        if(!Objects.isNull(checkCreditLimitDocs)){
            Optional<ShipmentDetails> shipmentsRow = shipmentDao.findById(request.getShipmentId());
            ShipmentDetails shipmentDetails = null;
            if(shipmentsRow.isPresent()) {
                shipmentDetails = shipmentsRow.get();
                var response = v1ServiceUtil.validateCreditLimit(modelMapper.map(shipmentDetails.getClient(), Parties.class), checkCreditLimitDocs, shipmentDetails.getGuid(), request.getTaskCreation());
                return ResponseHelper.buildSuccessResponse(response);
            }
            return ResponseHelper.buildFailedResponse("Shipment not exist for given id");
        } else {
            return ResponseHelper.buildFailedResponse("Please send a valid doc type for check credit limit.");
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getContainerListFromTrackingService(Long shipmentId, Long consolidationId) throws RunnerException {
        try {

            if (shipmentId == null && consolidationId == null) {
                throw new RunnerException("Empty request: please provide either ShipmentId or ConsolidationId");
            }

            Long effectiveShipmentId = getEffectiveShipmentId(shipmentId, consolidationId);

            ShipmentDetails shipmentDetails = shipmentDao.findById(effectiveShipmentId)
                    .orElseThrow(() -> new RunnerException("No shipment present for provided id " + shipmentId));

            TrackingServiceApiResponse trackingResponse = fetchTrackingDataByShipmentId(shipmentDetails.getShipmentId());
            TrackingServiceLiteContainerResponse response = convertToLiteContainerResponse(trackingResponse.getContainers());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            throw new RunnerException("Unexpected error occurred: ", e);
        }
    }

    private TrackingServiceLiteContainerResponse convertToLiteContainerResponse(List<Container> containers) {

        TrackingServiceLiteContainerResponse response = new TrackingServiceLiteContainerResponse();
        List<LiteContainer> liteContainers = new ArrayList<>();

        containers.forEach(container -> {

            TrackingServiceLiteContainerResponse.LiteContainer liteContainer = new TrackingServiceLiteContainerResponse.LiteContainer();

            liteContainer.setContainerNumber(container.getContainerNumber());
            liteContainer.setIdentifierType(container.getIdentifierType());
            liteContainer.setIdentifierValue(container.getIdentifierValue());
            if (container.getContainerBase() != null) {
                liteContainer.setType(container.getContainerBase().getType());
                liteContainer.setSize(container.getContainerBase().getSize());
                liteContainer.setTypeIsoCode(container.getContainerBase().getTypeIsoCode());
                liteContainer.setBolNumber(container.getContainerBase().getBolNumber());
                liteContainer.setBookingNumber(container.getContainerBase().getBookingNumber());
                liteContainer.setSealNumber(container.getContainerBase().getSealNumber());
                liteContainer.setMarks(container.getContainerBase().getMarks());
                liteContainer.setIncoterm(container.getContainerBase().getIncoterm());
                liteContainer.setShipper(container.getContainerBase().getShipper());
                liteContainer.setConsignee(container.getContainerBase().getConsignee());
                liteContainer.setWeight(container.getContainerBase().getWeight());
                liteContainer.setWeightUom(container.getContainerBase().getWeightUom());
                liteContainer.setNumberOfPackages(container.getContainerBase().getNumberOfPackages());
                liteContainer.setPackageType(container.getContainerBase().getPackageType());
                liteContainer.setReeferTemperature(container.getContainerBase().getReeferTemperature());
                liteContainer.setCommodity(container.getContainerBase().getCommodity());
                liteContainer.setLatitude(container.getContainerBase().getLatitude());
                liteContainer.setLongitude(container.getContainerBase().getLongitude());
                liteContainer.setLocation(container.getContainerBase().getLocation());
                liteContainer.setLocationUpdateTime(container.getContainerBase().getLocationUpdateTime());
            }
            liteContainers.add(liteContainer);
        });
        response.setContainers(liteContainers);
        return response;
    }

    private Long getEffectiveShipmentId(Long shipmentId, Long consolidationId) throws RunnerException {
        if (shipmentId != null) {
            return shipmentId;
        }

        List<ConsoleShipmentMapping> consoleShipmentMappingList = consoleShipmentMappingDao.findByConsolidationId(consolidationId);
        return consoleShipmentMappingList.stream()
                .findFirst()
                .map(ConsoleShipmentMapping::getShipmentId)
                .orElseThrow(() -> new RunnerException("No shipment present for provided consolidation id " + consolidationId));
    }

    private TrackingServiceApiResponse fetchTrackingDataByShipmentId(String shipmentId) throws RunnerException {
        try {
            return trackingServiceAdapter.fetchTrackingData(
                    TrackingRequest.builder().referenceNumber(shipmentId).build());
        } catch (Exception e) {
            throw new RunnerException("Error getting response from tracking api for shipment id: " + shipmentId + ", Details :" + e.getMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getDateTimeChangeUpdates(Long shipmentId) throws RunnerException {
        if(Objects.isNull(shipmentId))
            throw new RunnerException("shipment id can't be null");

        Optional<ShipmentDetails> optional = shipmentDao.findById(shipmentId);
        if(optional.isEmpty())
            throw new RunnerException("No shipment present for provided id");

        ShipmentDetails shipment = optional.get();

        TrackingServiceApiResponse trackingResponse = null;
        try {
            trackingResponse = trackingServiceAdapter.fetchTrackingData(
                    TrackingRequest.builder().referenceNumber(shipment.getShipmentId()).build());
        } catch (Exception ignored) {
            log.error("Error getting response from tracking api for date-time changes");
        }

        LocalDateTime trackingAta = null;
        LocalDateTime trackingAtd = null;
        LocalDateTime trackingEta = null;
        LocalDateTime trackingEtd = null;

        if(trackingResponse != null && trackingResponse.getContainers() != null && !trackingResponse.getContainers().isEmpty()
            && trackingResponse.getContainers().get(0).getJourney() != null) {
            trackingAta = Optional.ofNullable(trackingResponse.getContainers().get(0).getJourney().getPortOfArrivalAta()).map(TrackingServiceApiResponse.DateAndSources::getDateTime).orElse(null);
            trackingAtd = Optional.ofNullable(trackingResponse.getContainers().get(0).getJourney().getPortOfDepartureAtd()).map(TrackingServiceApiResponse.DateAndSources::getDateTime).orElse(null);
            trackingEta = Optional.ofNullable(trackingResponse.getContainers().get(0).getJourney().getPortOfArrivalEta()).map(TrackingServiceApiResponse.DateAndSources::getDateTime).orElse(null);
            trackingEtd = Optional.ofNullable(trackingResponse.getContainers().get(0).getJourney().getPortOfDepartureEtd()).map(TrackingServiceApiResponse.DateAndSources::getDateTime).orElse(null);
        }

        List<DateTimeChangeLog> shipmentDateLogs = dateTimeChangeLogService.getDateTimeChangeLog(shipmentId);
        Map<DateType, List<DateTimeChangeLog>> dateChangeLogMap = shipmentDateLogs.stream()
                .sorted(Comparator.comparing(DateTimeChangeLog::getUpdatedAt).reversed())
                .collect(Collectors.groupingBy(DateTimeChangeLog::getDateType)
        );

        UpstreamDateUpdateResponse upstreamDateUpdateResponse = new UpstreamDateUpdateResponse();

        //ata
        upstreamDateUpdateResponse.setAta(UpstreamDateUpdateResponse.DateAndLogResponse.builder().build());
        var ataChangeLogsResponse = jsonHelper.convertValueToList(dateChangeLogMap.get(DateType.ATA), DateTimeChangeLogResponse.class);
        upstreamDateUpdateResponse.getAta().setChangeLogs(ataChangeLogsResponse);
        if(trackingAta != null && !CommonUtils.areTimeStampsEqual(trackingAta, shipment.getCarrierDetails().getAta())) {
            upstreamDateUpdateResponse.getAta().setUpdatedDate(trackingAta);
        }
        //atd
        upstreamDateUpdateResponse.setAtd(UpstreamDateUpdateResponse.DateAndLogResponse.builder().build());
        var atdChangeLogsResponse = jsonHelper.convertValueToList(dateChangeLogMap.get(DateType.ATD), DateTimeChangeLogResponse.class);
        upstreamDateUpdateResponse.getAtd().setChangeLogs(atdChangeLogsResponse);
        if(trackingAtd != null && !CommonUtils.areTimeStampsEqual(trackingAtd, shipment.getCarrierDetails().getAtd())) {
            upstreamDateUpdateResponse.getAtd().setUpdatedDate(trackingAtd);
        }
        //eta
        upstreamDateUpdateResponse.setEta(UpstreamDateUpdateResponse.DateAndLogResponse.builder().build());
        var etaChangeLogsResponse = jsonHelper.convertValueToList(dateChangeLogMap.get(DateType.ETA), DateTimeChangeLogResponse.class);
        upstreamDateUpdateResponse.getEta().setChangeLogs(etaChangeLogsResponse);
        if(trackingEta != null && !CommonUtils.areTimeStampsEqual(trackingEta, shipment.getCarrierDetails().getEta())) {
             upstreamDateUpdateResponse.getEta().setUpdatedDate(trackingEta);
        }
        //etd
        upstreamDateUpdateResponse.setEtd(UpstreamDateUpdateResponse.DateAndLogResponse.builder().build());
        var etdChangeLogsResponse = jsonHelper.convertValueToList(dateChangeLogMap.get(DateType.ETD), DateTimeChangeLogResponse.class);
        upstreamDateUpdateResponse.getEtd().setChangeLogs(etdChangeLogsResponse);
        if(trackingEtd != null && !CommonUtils.areTimeStampsEqual(trackingEtd, shipment.getCarrierDetails().getEtd())) {
            upstreamDateUpdateResponse.getEtd().setUpdatedDate(trackingEtd);
        }

        return ResponseHelper.buildSuccessResponse(upstreamDateUpdateResponse);
    }

    @Override
    public ResponseEntity<IRunnerResponse> consoleShipmentList(CommonRequestModel commonRequestModel, Long consoleId, String consoleGuid, boolean isAttached, boolean getMasterData, boolean fromNte) throws AuthenticationException {
        validateRequiredParameters(consoleId, consoleGuid);

        Optional<ConsolidationDetails> consolidationDetails = getOptionalConsolidationDetails(consoleId, consoleGuid, fromNte);

        if (consolidationDetails.isEmpty()) {
            log.error(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        if(consoleId==null)
            consoleId = consolidationDetails.get().getId();

        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
        if (request == null) {
            log.error(ShipmentConstants.SHIPMENT_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException(ShipmentConstants.SHIPMENT_LIST_REQUEST_NULL_ERROR);
        }
        if (request.getFilterCriteria().isEmpty()) {
            request.setFilterCriteria(Arrays.asList(FilterCriteria.builder().innerFilter(new ArrayList<>()).build()));
        }

        Map<Long, ConsoleShipmentMapping> requestedTypeMap = new HashMap<>();
        // InterBranch Logic
        if (Boolean.TRUE.equals(consolidationDetails.get().getInterBranchConsole())) {
            commonUtils.setInterBranchContextForHub();
            if(!isAttached) {
                var consoleShipMappingList = consoleShipmentMappingDao.findByConsolidationIdAll(consoleId);
                if (consoleShipMappingList == null || consoleShipMappingList.isEmpty()) {
                    return ResponseHelper.buildListSuccessResponse(new ArrayList<>(), 1, 0);
                }
                requestedTypeMap = consoleShipMappingList.stream().collect(Collectors.toMap(ConsoleShipmentMapping::getShipmentId, Function.identity(), (existingValue, newValue) -> existingValue));
                List<Long> shipIds = consoleShipMappingList.stream().map(ConsoleShipmentMapping::getShipmentId).toList();
                CommonUtils.andCriteria("id", shipIds, "IN", request);
            } else {
                CommonUtils.andCriteria(CONSOLIDATION_ID, consoleId, "=", request);
            }
        } else {
            CommonUtils.andCriteria(CONSOLIDATION_ID, consoleId, "=", request);
        }
        var response = list(CommonRequestModel.buildRequest(request), getMasterData);
        processResponseList(response, requestedTypeMap);
        if(fromNte) {
            TenantContext.removeTenant();
        }
        return response;
    }

    private void validateRequiredParameters(Long consoleId, String consoleGuid) {
        if(consoleId ==null && consoleGuid ==null)
            throw new ValidationException("Required parameters missing: consoleId and consoleGuid");
    }

    private void processResponseList(ResponseEntity<IRunnerResponse> response, Map<Long, ConsoleShipmentMapping> requestedTypeMap) {
        if (response.getBody() instanceof RunnerListResponse<?> responseList) {
            for (var resp : responseList.getData()) {
                if (resp instanceof ShipmentListResponse shipmentListResponse
                        && requestedTypeMap.containsKey(shipmentListResponse.getId())
                        && !Objects.isNull(requestedTypeMap.get(shipmentListResponse.getId()).getRequestedType())) {
                    shipmentListResponse.setRequestedType(requestedTypeMap.get(shipmentListResponse.getId()).getRequestedType().getDescription());
                    shipmentListResponse.setRequestedBy(requestedTypeMap.get(shipmentListResponse.getId()).getCreatedBy());
                    shipmentListResponse.setRequestedOn(requestedTypeMap.get(shipmentListResponse.getId()).getCreatedAt());
                }
            }
        }
    }

    private Optional<ConsolidationDetails> getOptionalConsolidationDetails(Long consoleId, String consoleGuid, boolean fromNte) throws AuthenticationException {
        Optional<ConsolidationDetails> consolidationDetails;
        if(consoleId != null ){
            if (fromNte) {
                consolidationDetails = consolidationDetailsDao.findConsolidationByIdWithQuery(consoleId);
                if(consolidationDetails.isPresent()) {
                    this.isValidNte(consolidationDetails.get());
                    TenantContext.setCurrentTenant(consolidationDetails.get().getTenantId());
                }
            }
            else
                consolidationDetails = consolidationDetailsDao.findById(consoleId);
        } else {
            UUID guid = UUID.fromString(consoleGuid);
            consolidationDetails = consolidationDetailsDao.findByGuid(guid);
        }
        return consolidationDetails;
    }

    private boolean isValidNte(ConsolidationDetails consolidationDetails) throws AuthenticationException {
        List<TriangulationPartner> triangulationPartners = consolidationDetails.getTriangulationPartnerList();
        Long currentTenant = TenantContext.getCurrentTenant().longValue();
        if(Objects.equals(currentTenant, consolidationDetails.getTenantId()))
            return false;
        if (
                (triangulationPartners == null
                && !Objects.equals(consolidationDetails.getTriangulationPartner(), TenantContext.getCurrentTenant().longValue())
                && !Objects.equals(consolidationDetails.getReceivingBranch(), TenantContext.getCurrentTenant().longValue()))
                ||
                ((triangulationPartners == null || triangulationPartners.stream().filter(Objects::nonNull).noneMatch(tp -> Objects.equals(tp.getTriangulationPartner(), currentTenant)))
                && !Objects.equals(consolidationDetails.getReceivingBranch(), currentTenant))
        ) {
            throw new AuthenticationException(Constants.NOT_ALLOWED_TO_VIEW_CONSOLIDATION_FOR_NTE);
        }
        return true;
    }

    @Override
    public ResponseEntity<IRunnerResponse> getAllShipments(Long consoleId) {
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(consoleId);
        Long attachedShipmentCurrentBranchCount = 0L;
        Long attachedShipmentInterBranchCount = 0L;
        Long pendingAttachmentCount = 0L;
        AllShipmentCountResponse allShipmentCountResponse = new AllShipmentCountResponse();
        if(consolidationDetails.isPresent()) {
            if(Boolean.TRUE.equals(consolidationDetails.get().getInterBranchConsole())) {
                commonUtils.setInterBranchContextForHub();
            }
            List<ConsoleShipmentMapping> consoleShipmentMappingList = consoleShipmentMappingDao.findByConsolidationIdAll(consoleId);
            for(ConsoleShipmentMapping consoleShipmentMapping: consoleShipmentMappingList) {
                if(consoleShipmentMapping.getRequestedType() == null) {
                    attachedShipmentCurrentBranchCount++;
                } else if(consoleShipmentMapping.getRequestedType().equals(ShipmentRequestedType.APPROVE)) {
                    attachedShipmentInterBranchCount++;
                } else if(consoleShipmentMapping.getRequestedType().equals(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED) || consoleShipmentMapping.getRequestedType().equals(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED)) {
                    pendingAttachmentCount++;
                }
            }
            allShipmentCountResponse.setAttachedShipmentCurrentBranchCount(attachedShipmentCurrentBranchCount);
            allShipmentCountResponse.setAttachedShipmentInterBranchCount(attachedShipmentInterBranchCount);
            allShipmentCountResponse.setPendingAttachmentCount(pendingAttachmentCount);
        } else {
            log.error(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        return ResponseHelper.buildSuccessResponse(allShipmentCountResponse);
    }

    public ResponseEntity<IRunnerResponse> getLatestCargoDeliveryDate(Long consoleId) {
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(consoleId);
        LatestCargoDeliveryInfo latestCargoDeliveryInfo = new LatestCargoDeliveryInfo();
        LocalDateTime latestCargoDeliveryDate = null;
        if(consolidationDetails.isPresent()) {
            Set<ShipmentDetails> listOfShipmentsAttachedToConsole = consolidationDetails.get().getShipmentsList();
            latestCargoDeliveryDate = getLatestCargoDeliveryDateHelper(listOfShipmentsAttachedToConsole);
            latestCargoDeliveryInfo.setLatestCargoDeliveryDate(latestCargoDeliveryDate);
        } else {
            log.error(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        return ResponseHelper.buildSuccessResponse(latestCargoDeliveryInfo);
    }

    private LocalDateTime getLatestCargoDeliveryDateHelper(Set<ShipmentDetails> listOfShipmentsAttachedToConsole) {
        return listOfShipmentsAttachedToConsole.stream()
                .map(ShipmentDetails::getCargoDeliveryDate)
                .filter(Objects::nonNull)
                .max(Comparator.naturalOrder())
                .orElse(null);
    }

    private void fetchShipmentsAndConsolidationsForPushRequestEmails(Set<Integer> tenantIds, Set<String> usernamesList, List<Long> shipmentIds, ConsolidationDetails consolidationDetails,
                                                                     List<ShipmentDetails> shipmentDetails, List<ConsolidationDetails> otherConsolidationDetails,
                                                                     List<ConsoleShipmentMapping> consoleShipmentMappings, Map<Long, String> requestedUsernameMap) {
        // fetching shipments
        ListCommonRequest listCommonRequest = constructListCommonRequest(ID, shipmentIds, "IN");
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class);
        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(pair.getLeft(), pair.getRight());
        for(ShipmentDetails shipmentDetails1 : shipmentDetailsPage.getContent()) {
            tenantIds.add(shipmentDetails1.getTenantId());
            usernamesList.add(shipmentDetails1.getCreatedBy());
            usernamesList.add(shipmentDetails1.getAssignedTo());
            shipmentDetails.add(shipmentDetails1);
        }

        // fetching other consolidations
        List<Long> otherConsoleIds = new ArrayList<>();
        for(ConsoleShipmentMapping consoleShipmentMapping : consoleShipmentMappings) {
            if(!Boolean.TRUE.equals(consoleShipmentMapping.getIsAttachmentDone())) {
                otherConsoleIds.add(consoleShipmentMapping.getConsolidationId());
            } else if(shipmentIds.contains(consoleShipmentMapping.getShipmentId()) && Objects.equals(consoleShipmentMapping.getConsolidationId(), consolidationDetails.getId())){
                requestedUsernameMap.put(consoleShipmentMapping.getShipmentId(), consoleShipmentMapping.getCreatedBy());
            }
            usernamesList.add(consoleShipmentMapping.getCreatedBy());
        }
        Page<ConsolidationDetails> consolidationDetailsPage = null;
        if(!otherConsoleIds.isEmpty()) {
            listCommonRequest = constructListCommonRequest(ID, otherConsoleIds, "IN");
            Pair<Specification<ConsolidationDetails>, Pageable> pair3 = fetchData(listCommonRequest, ConsolidationDetails.class);
            consolidationDetailsPage = consolidationDetailsDao.findAll(pair3.getLeft(), pair3.getRight());
            for(ConsolidationDetails consolidationDetails1 : consolidationDetailsPage.getContent()) {
                tenantIds.add(consolidationDetails1.getTenantId());
                usernamesList.add(consolidationDetails1.getCreatedBy());
                otherConsolidationDetails.add(consolidationDetails1);
            }
        }
    }

    private void constructAndSendEmailsForPushRequestAccept(List<ShipmentDetails> shipmentDetails, ConsolidationDetails consolidationDetails,
                                                            List<ConsolidationDetails> otherConsolidationDetails, List<ConsoleShipmentMapping> consoleShipmentMappings,
                                                            Set<ShipmentRequestedType> shipmentRequestedTypes,
                                                            Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap,
                                                            Map<String, String> usernameEmailsMap, Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap,
                                                            Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests, Map<Long, String> requestedUsernameMap) {
        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
        if(shipmentDetails != null && !shipmentDetails.isEmpty()) {
            shipmentDetailsMap = shipmentDetails.stream().collect(Collectors.toMap(BaseEntity::getId, e -> e));
            shipmentDetails.forEach(shipment -> {
                try {
                    commonUtils.sendEmailForPullPushRequestStatus(shipment, consolidationDetails, SHIPMENT_PUSH_ACCEPTED, null, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, requestedUsernameMap.get(shipment.getId()), null);
                } catch (Exception e) {
                    log.error(ERROR_WHILE_SENDING_EMAIL);
                }
            });
        }
        if(!otherConsolidationDetails.isEmpty()) {
            Map<Long, ConsolidationDetails> finalConsolidationDetailsMap = otherConsolidationDetails.stream().collect(Collectors.toMap(BaseEntity::getId, y -> y));
            Map<Long, ShipmentDetails> finalShipmentDetailsMap = shipmentDetailsMap;
            consoleShipmentMappings.stream().filter(e -> !Boolean.TRUE.equals(e.getIsAttachmentDone())).forEach(consoleShipmentMapping -> {
                try {
                    if(finalConsolidationDetailsMap.containsKey(consoleShipmentMapping.getConsolidationId()) && finalShipmentDetailsMap.containsKey(consoleShipmentMapping.getShipmentId())) {
                        sendPullPushRequestStatusEmail(shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, emailTemplatesRequests, consoleShipmentMapping, finalShipmentDetailsMap, finalConsolidationDetailsMap);
                    }
                } catch (Exception e) {
                    log.error(ERROR_WHILE_SENDING_EMAIL);
                }
            });
        }
    }

    private void sendPullPushRequestStatusEmail(Set<ShipmentRequestedType> shipmentRequestedTypes, Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap, Map<String, String> usernameEmailsMap, Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap, Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests, ConsoleShipmentMapping consoleShipmentMapping, Map<Long, ShipmentDetails> finalShipmentDetailsMap, Map<Long, ConsolidationDetails> finalConsolidationDetailsMap) throws Exception {
        if(consoleShipmentMapping.getRequestedType() == SHIPMENT_PUSH_REQUESTED)
            commonUtils.sendEmailForPullPushRequestStatus(finalShipmentDetailsMap.get(consoleShipmentMapping.getShipmentId()), finalConsolidationDetailsMap.get(consoleShipmentMapping.getConsolidationId()), SHIPMENT_PUSH_REJECTED, AUTO_REJECTION_REMARK, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, consoleShipmentMapping.getCreatedBy(), null);
        else
            commonUtils.sendEmailForPullPushRequestStatus(finalShipmentDetailsMap.get(consoleShipmentMapping.getShipmentId()), finalConsolidationDetailsMap.get(consoleShipmentMapping.getConsolidationId()), SHIPMENT_PULL_REJECTED, AUTO_REJECTION_REMARK, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, consoleShipmentMapping.getCreatedBy(), null);
    }

    public void sendEmailsForPushRequestAccept(ConsolidationDetails consolidationDetails, List<Long> shipmentIds, Set<ShipmentRequestedType> shipmentRequestedTypes, List<ConsoleShipmentMapping> consoleShipmentMappings) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests =  new EnumMap<>(ShipmentRequestedType.class);
        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();
        Map<String, String> usernameEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> usernamesList = new HashSet<>();
        List<ShipmentDetails> shipmentDetails = new ArrayList<>();
        List<ConsolidationDetails> otherConsolidationDetails = new ArrayList<>();
        Map<Long, String> requestedUsernameMap = new HashMap<>();

        // fetching data from db
        fetchShipmentsAndConsolidationsForPushRequestEmails(tenantIds, usernamesList, shipmentIds, consolidationDetails, shipmentDetails, otherConsolidationDetails, consoleShipmentMappings, requestedUsernameMap);

        // making v1 calls for master data
        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesRequests)), executorService);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getCarriersData(Stream.of(consolidationDetails.getCarrierDetails().getShippingLine()).filter(Objects::nonNull).toList(), carrierMasterDataMap)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUnLocationsData(Stream.of(consolidationDetails.getCarrierDetails().getOriginPort(), consolidationDetails.getCarrierDetails().getDestinationPort()).filter(Objects::nonNull).toList(), unLocMap)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(usernamesList, usernameEmailsMap)), executorService);

        CompletableFuture.allOf(emailTemplateFuture, carrierFuture, unLocationsFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();

        // constructing and sending emails
        constructAndSendEmailsForPushRequestAccept(shipmentDetails, consolidationDetails, otherConsolidationDetails, consoleShipmentMappings, shipmentRequestedTypes,
                unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, emailTemplatesRequests, requestedUsernameMap);
    }

    private String fetchShipmentsAndConsolidationsForPullRequestEmails(Set<Integer> tenantIds, Set<String> usernamesList, Long consoleId, Long shipmentId,
                                                                     ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                                                     List<ConsoleShipmentMapping> consoleShipmentMappings, List<ConsolidationDetails> otherConsolidationdetails) {
        // fetching shipment and console
        tenantIds.add(consolidationDetails.getTenantId());
        usernamesList.add(shipmentDetails.getCreatedBy());
        usernamesList.add(shipmentDetails.getAssignedTo());
        usernamesList.add(consolidationDetails.getCreatedBy());

        // fetching other consolidations
        List<Long> otherConsoleIds = new ArrayList<>();
        String requestedUsername = null;
        for(ConsoleShipmentMapping consoleShipmentMapping : consoleShipmentMappings) {
            if(!Boolean.TRUE.equals(consoleShipmentMapping.getIsAttachmentDone())) {
                otherConsoleIds.add(consoleShipmentMapping.getConsolidationId());
            } else if(Objects.equals(consoleShipmentMapping.getShipmentId(), shipmentId) && Objects.equals(consoleShipmentMapping.getConsolidationId(), consoleId)){
                requestedUsername = consoleShipmentMapping.getCreatedBy();
            }
            usernamesList.add(consoleShipmentMapping.getCreatedBy());
        }
        ListCommonRequest listCommonRequest = constructListCommonRequest(ID, otherConsoleIds, "IN");
        Pair<Specification<ConsolidationDetails>, Pageable> pair3 = fetchData(listCommonRequest, ConsolidationDetails.class);
        Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(pair3.getLeft(), pair3.getRight());
        for(ConsolidationDetails consolidationDetails1 : consolidationDetailsPage.getContent()) {
            usernamesList.add(consolidationDetails1.getCreatedBy());
            tenantIds.add(consolidationDetails1.getTenantId());
            otherConsolidationdetails.add(consolidationDetails1);
        }
        return requestedUsername;
    }

    public void sendEmailsForPullRequestAccept(Long consoleId, Long shipmentId, Set<ShipmentRequestedType> shipmentRequestedTypes, List<ConsoleShipmentMapping> consoleShipmentMappings) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests =  new EnumMap<>(ShipmentRequestedType.class);
        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();
        Map<String, String> usernameEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> usernamesList = new HashSet<>();
        List<ConsolidationDetails> otherConsolidationdetails = new ArrayList<>();

        // fetching data from db
        ShipmentDetails shipmentDetails = shipmentDao.findById(shipmentId).get();
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findConsolidationsById(consoleId);
        String requestedUsername = fetchShipmentsAndConsolidationsForPullRequestEmails(tenantIds, usernamesList, consoleId, shipmentId, shipmentDetails, consolidationDetails, consoleShipmentMappings, otherConsolidationdetails);

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesRequests)), executorService);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getCarriersData(Stream.of(shipmentDetails.getCarrierDetails().getShippingLine()).filter(Objects::nonNull).toList(), carrierMasterDataMap)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUnLocationsData(Stream.of(shipmentDetails.getCarrierDetails().getOriginPort(), shipmentDetails.getCarrierDetails().getDestinationPort()).filter(Objects::nonNull).toList(), unLocMap)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(usernamesList, usernameEmailsMap)), executorService);
        CompletableFuture.allOf(emailTemplateFuture, carrierFuture, unLocationsFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();

        try {
            commonUtils.sendEmailForPullPushRequestStatus(shipmentDetails, consolidationDetails, SHIPMENT_PULL_ACCEPTED, null, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, requestedUsername, null);
        } catch (Exception e) {
            log.error(ERROR_WHILE_SENDING_EMAIL);
        }
        if(!otherConsolidationdetails.isEmpty()) {
            Map<Long, ConsolidationDetails> finalConsolidationDetailsMap = otherConsolidationdetails.stream().collect(Collectors.toMap(BaseEntity::getId, y -> y));
            consoleShipmentMappings.stream().filter(e -> !Boolean.TRUE.equals(e.getIsAttachmentDone())).forEach(consoleShipmentMapping -> {
                try {
                    if(finalConsolidationDetailsMap.containsKey(consoleShipmentMapping.getConsolidationId())) {
                        if(consoleShipmentMapping.getRequestedType() == SHIPMENT_PUSH_REQUESTED)
                            commonUtils.sendEmailForPullPushRequestStatus(shipmentDetails, finalConsolidationDetailsMap.get(consoleShipmentMapping.getConsolidationId()), SHIPMENT_PUSH_REJECTED, AUTO_REJECTION_REMARK, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, consoleShipmentMapping.getCreatedBy(), null);
                        else
                            commonUtils.sendEmailForPullPushRequestStatus(shipmentDetails, finalConsolidationDetailsMap.get(consoleShipmentMapping.getConsolidationId()), SHIPMENT_PULL_REJECTED, AUTO_REJECTION_REMARK, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, consoleShipmentMapping.getCreatedBy(), null);
                    }
                } catch (Exception e) {
                    log.error(ERROR_WHILE_SENDING_EMAIL);
                }
            });
        }

    }

    public void sendEmailForPushRequestReject(ConsolidationDetails consolidationDetails, List<Long> shipmentIds, Set<ShipmentRequestedType> shipmentRequestedTypes, String rejectRemarks, List<ConsoleShipmentMapping> consoleShipmentMappings) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests =  new EnumMap<>(ShipmentRequestedType.class);
        Map<String, String> usernameEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> usernamesList = new HashSet<>();

        ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.ID, shipmentIds, "IN");
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class);
        Page<ShipmentDetails> shipmentDetails = shipmentDao.findAll(pair.getLeft(), pair.getRight());
        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
        for(ShipmentDetails shipmentDetails1 : shipmentDetails.getContent()) {
            shipmentDetailsMap.put(shipmentDetails1.getId(), shipmentDetails1);
            tenantIds.add(shipmentDetails1.getTenantId());
            usernamesList.add(shipmentDetails1.getCreatedBy());
            usernamesList.add(shipmentDetails1.getAssignedTo());
        }
        if(shipmentDetails != null && !shipmentDetails.getContent().isEmpty())
            shipmentDetailsMap = shipmentDetails.stream().collect(Collectors.toMap(BaseEntity::getId, e -> e));

        Map<Long, String> shipmentRequestUserMap = new HashMap<>();
        for(ConsoleShipmentMapping consoleShipmentMapping : consoleShipmentMappings) {
            shipmentRequestUserMap.put(consoleShipmentMapping.getShipmentId(), consoleShipmentMapping.getCreatedBy());
            usernamesList.add(consoleShipmentMapping.getCreatedBy());
        }

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesRequests)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(usernamesList, usernameEmailsMap)), executorService);
        CompletableFuture.allOf(emailTemplateFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();

        for(Long shipId : shipmentIds) {
            try {
                commonUtils.sendEmailForPullPushRequestStatus(shipmentDetailsMap.get(shipId), consolidationDetails, SHIPMENT_PUSH_REJECTED, rejectRemarks, emailTemplatesRequests, shipmentRequestedTypes, null, null, usernameEmailsMap, v1TenantSettingsMap, shipmentRequestUserMap.get(shipId), null);
            } catch (Exception e) {
                log.error(ERROR_WHILE_SENDING_EMAIL);
            }
        }
    }

    public void sendEmailForPullRequestReject(Long shipmentId, List<Long> consoleIds, Set<ShipmentRequestedType> shipmentRequestedTypes, String rejectRemarks, List<ConsoleShipmentMapping> consoleShipmentMappings) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests =  new EnumMap<>(ShipmentRequestedType.class);
        Map<String, String> usernameEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> usernamesList = new HashSet<>();

        // fetching shipment and consolidations
        ListCommonRequest listCommonRequest = constructListCommonRequest(ID, consoleIds, "IN");
        Pair<Specification<ConsolidationDetails>, Pageable> pair = fetchData(listCommonRequest, ConsolidationDetails.class);
        Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(pair.getLeft(), pair.getRight());
        Map<Long, ConsolidationDetails> consolidationDetailsMap = new HashMap<>();
        for(ConsolidationDetails consolidationDetails : consolidationDetailsPage.getContent()) {
            consolidationDetailsMap.put(consolidationDetails.getId(), consolidationDetails);
            tenantIds.add(consolidationDetails.getTenantId());
            usernamesList.add(consolidationDetails.getCreatedBy());
        }

        ShipmentDetails shipmentDetails = shipmentDao.findById(shipmentId).get();
        usernamesList.add(shipmentDetails.getCreatedBy());
        usernamesList.add(shipmentDetails.getAssignedTo());

        Map<Long, String> consoleRequestUserMap = new HashMap<>();
        for(ConsoleShipmentMapping consoleShipmentMapping : consoleShipmentMappings) {
            consoleRequestUserMap.put(consoleShipmentMapping.getConsolidationId(), consoleShipmentMapping.getCreatedBy());
            usernamesList.add(consoleShipmentMapping.getCreatedBy());
        }

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesRequests)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(usernamesList, usernameEmailsMap)), executorService);
        CompletableFuture.allOf(emailTemplateFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();

        for(Long consoleId : consoleIds) {
            try {
                commonUtils.sendEmailForPullPushRequestStatus(shipmentDetails, consolidationDetailsMap.get(consoleId), SHIPMENT_PULL_REJECTED, rejectRemarks, emailTemplatesRequests, shipmentRequestedTypes, null, null, usernameEmailsMap, v1TenantSettingsMap, consoleRequestUserMap.get(consoleId), null);
            } catch (Exception e) {
                log.error(ERROR_WHILE_SENDING_EMAIL);
            }
        }
    }

    public void sendEmailForPushRequested(Long shipmentId, Long consoleId, Set<ShipmentRequestedType> shipmentRequestedTypes) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests =  new EnumMap<>(ShipmentRequestedType.class);
        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();
        Map<String, String> usernameEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> usernamesList = new HashSet<>();

        ShipmentDetails shipmentDetails = shipmentDao.findById(shipmentId).get();
        setColoadingStation(shipmentDetails);
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(consoleId).get();

        usernamesList.add(shipmentDetails.getCreatedBy());
        usernamesList.add(shipmentDetails.getAssignedTo());
        usernamesList.add(consolidationDetails.getCreatedBy());
        tenantIds.add(consolidationDetails.getTenantId());

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesRequests)), executorService);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getCarriersData(Stream.of(shipmentDetails.getCarrierDetails().getShippingLine()).filter(Objects::nonNull).toList(), carrierMasterDataMap)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUnLocationsData(Stream.of(shipmentDetails.getCarrierDetails().getOriginPort(), shipmentDetails.getCarrierDetails().getDestinationPort()).filter(Objects::nonNull).toList(), unLocMap)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(usernamesList, usernameEmailsMap)), executorService);
        CompletableFuture.allOf(emailTemplateFuture, carrierFuture, unLocationsFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();

        try {
            commonUtils.sendEmailForPullPushRequestStatus(shipmentDetails, consolidationDetails, SHIPMENT_PUSH_REQUESTED, null, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, null, null);
        } catch (Exception e) {
            log.error("Error while sending email");
        }
    }

    public void sendEmailForPushRequestWithdrawl(Long shipmentId, List<Long> consolidationIds, Set<ShipmentRequestedType> shipmentRequestedTypes, String withdrawRemarks) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests =  new EnumMap<>(ShipmentRequestedType.class);
        Map<String, String> usernameEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> usernamesList = new HashSet<>();
        Map<Integer, TenantModel> tenantsModelMap = new HashMap<>();

        List<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findConsolidationsByIds(new HashSet<>(consolidationIds));
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(shipmentId);
        Map<Long, ConsolidationDetails> consolidationDetailsMap = new HashMap<>();
        if(!listIsNullOrEmpty(consolidationDetails)) {
            for(ConsolidationDetails consolidationDetails1 : consolidationDetails) {
                consolidationDetailsMap.put(consolidationDetails1.getId(), consolidationDetails1);
                tenantIds.add(consolidationDetails1.getTenantId());
                usernamesList.add(consolidationDetails1.getCreatedBy());
            }
        }
        usernamesList.add(shipmentDetails.get().getCreatedBy());
        usernamesList.add(shipmentDetails.get().getAssignedTo());
        tenantIds.add(shipmentDetails.get().getTenantId());

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesRequests)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(usernamesList, usernameEmailsMap)), executorService);
        var tenantsDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettingsAndTenantsData(tenantIds, v1TenantSettingsMap, tenantsModelMap)), executorService);
        CompletableFuture.allOf(emailTemplateFuture, tenantsDataFuture, userEmailsFuture).join();
        for(ConsolidationDetails consolidationDetails1 : consolidationDetails){
            try {
                commonUtils.sendEmailForPullPushRequestStatus(shipmentDetails.get(), consolidationDetails1, SHIPMENT_PUSH_WITHDRAW, withdrawRemarks, emailTemplatesRequests, shipmentRequestedTypes, null, null, usernameEmailsMap, v1TenantSettingsMap, null, tenantsModelMap);
            } catch (Exception e) {
                log.error(ERROR_WHILE_SENDING_EMAIL);
            }
        }
    }

    public void sendEmailForPullRequestWithdrawal(ConsolidationDetails consolidationDetails, List<Long> shipmentIds, Set<ShipmentRequestedType> shipmentRequestedTypes, String withdrawRemarks) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests =  new EnumMap<>(ShipmentRequestedType.class);
        Map<String, String> usernameEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> usernamesList = new HashSet<>();
        Map<Integer, TenantModel> tenantsModelMap = new HashMap<>();

        List<ShipmentDetails> shipmentDetails = shipmentDao.findShipmentsByIds(new HashSet<>(shipmentIds));
        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
        if(!listIsNullOrEmpty(shipmentDetails)) {
            for(ShipmentDetails shipmentDetails1 : shipmentDetails) {
                shipmentDetailsMap.put(shipmentDetails1.getId(), shipmentDetails1);
                tenantIds.add(shipmentDetails1.getTenantId());
                usernamesList.add(shipmentDetails1.getCreatedBy());
                usernamesList.add(shipmentDetails1.getAssignedTo());
            }
        }
        usernamesList.add(consolidationDetails.getCreatedBy());
        tenantIds.add(consolidationDetails.getTenantId());

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesRequests)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(usernamesList, usernameEmailsMap)), executorService);
        var tenantsDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettingsAndTenantsData(tenantIds, v1TenantSettingsMap, tenantsModelMap)), executorService);
        CompletableFuture.allOf(emailTemplateFuture, tenantsDataFuture, userEmailsFuture).join();

        for(Long shipId : shipmentIds) {
            try {
                commonUtils.sendEmailForPullPushRequestStatus(shipmentDetailsMap.get(shipId), consolidationDetails, SHIPMENT_PULL_WITHDRAW, withdrawRemarks, emailTemplatesRequests, shipmentRequestedTypes, null, null, usernameEmailsMap, v1TenantSettingsMap, null, tenantsModelMap);
            } catch (Exception e) {
                log.error(ERROR_WHILE_SENDING_EMAIL);
            }
        }
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> updateShipments(UpdateConsoleShipmentRequest request) throws RunnerException {
        Set<ShipmentRequestedType> shipmentRequestedTypes = new HashSet<>();
        if (isForHubRequest(request)) {
            processHubRequest(request, shipmentRequestedTypes);
        } else {
            processShipmentRequest(request, shipmentRequestedTypes);
        }
        String warning = getWarningMsg(shipmentRequestedTypes);
        return ResponseHelper.buildSuccessResponseWithWarning(warning);
    }

    private boolean checkIfAlreadyPushRequested(ShipmentDetails oldEntity) {
        Integer allMappingsCount = consoleShipmentMappingDao.countAllStateMappings(oldEntity.getId());
        return allMappingsCount > 0;
    }

    @Override
    public ResponseEntity<IRunnerResponse> validateInvoicePosting(InvoicePostingValidationRequest request) {
        Set<UUID> shipmentGuids = request.getShipmentGuids().stream().filter(ObjectUtils::isNotEmpty)
                .map(UUID::fromString).collect(Collectors.toSet());

        List<ShipmentDetails> shipments = shipmentDao.findShipmentsByGuids(shipmentGuids);
        List<InvoicePostingValidationResponse> responses = new ArrayList<>();

        shipments.forEach(shipment -> {
            List<ModuleValidationFieldType> missingFields = new ArrayList<>();

            if (Constants.TRANSPORT_MODE_SEA.equalsIgnoreCase(shipment.getTransportMode())) {
                if (Constants.DIRECTION_EXP.equalsIgnoreCase(shipment.getDirection())
                        && (Constants.CARGO_TYPE_FCL.equalsIgnoreCase(shipment.getShipmentType())
                        || Constants.SHIPMENT_TYPE_LCL.equalsIgnoreCase(shipment.getShipmentType()))) {
                    if (Constants.SHIPMENT_TYPE_DRT.equalsIgnoreCase(shipment.getJobType())) {

                        validateCarrierDetails(shipment, missingFields);
                        validateContainerDetails(shipment, missingFields);
                        validateMblDetails(shipment, missingFields);

                    } else if (ObjectUtils.isNotEmpty(shipment.getJobType())) {

                        validateMblDetails(shipment, missingFields);

                    }
                }
            } else if (Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(shipment.getTransportMode())
                    && Constants.DIRECTION_EXP.equalsIgnoreCase(shipment.getDirection())
                    && Constants.SHIPMENT_TYPE_LSE.equalsIgnoreCase(shipment.getShipmentType())
                    && ObjectUtils.isNotEmpty(shipment.getJobType())) {

                validateCarrierDetails(shipment, missingFields);
                validateMawbDetails(shipment, missingFields);

            }

            responses.add(InvoicePostingValidationResponse.builder()
                    .shipmentGuid(shipment.getGuid().toString())
                    .missingFields(missingFields).build());
        });

        return ResponseHelper.buildSuccessResponse(responses);

    }

    public void validateContainerDetails(ShipmentDetails shipment, List<ModuleValidationFieldType> missingFields) {
        if (ObjectUtils.isEmpty(shipment.getContainersList()) || !isContainerNumberPresent(shipment.getContainersList())) {
            missingFields.add(ModuleValidationFieldType.CONTAINER_DETAILS);
        }
    }

    public void validateCarrierDetails(ShipmentDetails shipment, List<ModuleValidationFieldType> missingFields) {
        CarrierDetails carrierDetails = shipment.getCarrierDetails();

        if (ObjectUtils.isEmpty(carrierDetails)) {
            missingFields.add(ModuleValidationFieldType.CARRIER);
            return;
        }

        if (ObjectUtils.isEmpty(carrierDetails.getShippingLine())) {
            missingFields.add(ModuleValidationFieldType.CARRIER);
        }
        if (ObjectUtils.isEmpty(carrierDetails.getEtd())) {
            missingFields.add(ModuleValidationFieldType.CARRIER_ETD);
        }
        if (ObjectUtils.isEmpty(carrierDetails.getEta())) {
            missingFields.add(ModuleValidationFieldType.CARRIER_ETA);
        }
    }

    public void validateHblContainerNumberCondition(ShipmentDetails shipmentDetails){
        if(Objects.isNull(shipmentDetails.getContainersList()) || shipmentDetails.getContainersList().isEmpty()){
            throw new ValidationException("Container number is Mandatory for HBL Generation, please assign the container number to all the containers & packs in the shipment");
        }
        if(!Objects.isNull(shipmentDetails.getContainersList()) ) {
            List<Containers> containers = shipmentDetails.getContainersList().stream().filter(c -> StringUtility.isEmpty(c.getContainerNumber())).toList();
            if (!containers.isEmpty())
                throw new ValidationException("Please assign container number to all the containers before generating the HBL.");
        }

        if(!Objects.isNull(shipmentDetails.getPackingList())) {
            var packsList = shipmentDetails.getPackingList().stream().filter(x -> Objects.isNull(x.getContainerId())).toList();
            if(!packsList.isEmpty()){
                throw new ValidationException("Container Number is Mandatory for HBL Generation, please assign the container number for all the packages in the shipment.");
            }
        }


    }

    public void validateMblDetails(ShipmentDetails shipment, List<ModuleValidationFieldType> missingFields) {
        if (ObjectUtils.isEmpty(shipment.getMasterBill())) {
            missingFields.add(ModuleValidationFieldType.MBL_DETAILS);
        }
    }

    public void validateMawbDetails(ShipmentDetails shipment, List<ModuleValidationFieldType> missingFields) {
        if (ObjectUtils.isEmpty(shipment.getMasterBill())) {
            missingFields.add(ModuleValidationFieldType.MAWB_DETAILS);
        }
    }

    private boolean isContainerNumberPresent(Set<Containers> containersList) {
        return containersList.stream().allMatch(container -> container.getContainerNumber() != null);
    }

    private boolean isForHubRequest(UpdateConsoleShipmentRequest request) {
        return request.isForHub();
    }

    private void processHubRequest(UpdateConsoleShipmentRequest updateConsoleShipmentRequest, Set<ShipmentRequestedType> shipmentRequestedTypes) {
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(updateConsoleShipmentRequest.getConsoleId());
        if (consolidationDetails.isPresent()) {
            if (Boolean.TRUE.equals(updateConsoleShipmentRequest.isForHub()) && Boolean.TRUE.equals(consolidationDetails.get().getInterBranchConsole())) {
                commonUtils.setInterBranchContextForHub();
            }
            if (ShipmentRequestedType.APPROVE.equals(updateConsoleShipmentRequest.getShipmentRequestedType())) { // one console multiple shipments
                updateConsoleShipmentRequest.getListOfShipments().stream().forEach(shipmentId -> {
                    try {
                        consolidationService.attachShipments(updateConsoleShipmentRequest.getShipmentRequestedType(), updateConsoleShipmentRequest.getConsoleId(), List.of(shipmentId), true);
                    } catch (RunnerException e) {
                        log.error("Error while attaching shipments: {}", e.getMessage(), e);
                        throw new BillingException(e.getMessage());
                    }
                });
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, updateConsoleShipmentRequest.getListOfShipments(), "IN");
                Pair<Specification<ConsoleShipmentMapping>, Pageable> pair = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
                List<ConsoleShipmentMapping> consoleShipmentMappingsForEmails = jsonHelper.convertValueToList(consoleShipmentMappingDao.findAll(pair.getLeft(), pair.getRight()).getContent(), ConsoleShipmentMapping.class);

                consoleShipmentMappingDao.deletePendingStateByShipmentIds(updateConsoleShipmentRequest.getListOfShipments());
                // one console and list of approved shipments for shipment push accepted from console
                // for each shipment pending multiple consolidation auto rejections (shipment push and shipment pull both got rejected)
                sendEmailsForPushRequestAccept(consolidationDetails.get(), updateConsoleShipmentRequest.getListOfShipments(), shipmentRequestedTypes, consoleShipmentMappingsForEmails);
            } else if (ShipmentRequestedType.REJECT.equals(updateConsoleShipmentRequest.getShipmentRequestedType()) || ShipmentRequestedType.WITHDRAW.equals(updateConsoleShipmentRequest.getShipmentRequestedType())) {
                ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, updateConsoleShipmentRequest.getListOfShipments(), "IN", null);
                listCommonRequest = andCriteria(CONSOLIDATION_ID, updateConsoleShipmentRequest.getConsoleId(), "=", listCommonRequest);
                Pair<Specification<ConsoleShipmentMapping>, Pageable> pair2 = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
                List<ConsoleShipmentMapping> consoleShipmentMappings = jsonHelper.convertValueToList(consoleShipmentMappingDao.findAll(pair2.getLeft(), pair2.getRight()).getContent(), ConsoleShipmentMapping.class);
                updateConsoleShipmentRequest.getListOfShipments().stream().forEach(shipmentId -> consoleShipmentMappingDao.deletePendingStateByConsoleIdAndShipmentId(updateConsoleShipmentRequest.getConsoleId(), shipmentId));
                // one console and multiple shipments (shipment push rejected)
                sendRejectionEmails(updateConsoleShipmentRequest, shipmentRequestedTypes, consolidationDetails.get(), consoleShipmentMappings);
            }
        } else {
            log.error(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
    }

    private void sendRejectionEmails(UpdateConsoleShipmentRequest updateConsoleShipmentRequest, Set<ShipmentRequestedType> shipmentRequestedTypes, ConsolidationDetails consolidationDetails, List<ConsoleShipmentMapping> consoleShipmentMappings) {
        if(ShipmentRequestedType.REJECT.equals(updateConsoleShipmentRequest.getShipmentRequestedType())) {
            sendEmailForPushRequestReject(consolidationDetails, updateConsoleShipmentRequest.getListOfShipments(), shipmentRequestedTypes, updateConsoleShipmentRequest.getRejectRemarks(), consoleShipmentMappings);
        } else if(ShipmentRequestedType.WITHDRAW.equals(updateConsoleShipmentRequest.getShipmentRequestedType())) {
            sendEmailForPullRequestWithdrawal(consolidationDetails, updateConsoleShipmentRequest.getListOfShipments(), shipmentRequestedTypes, updateConsoleShipmentRequest.getRejectRemarks());
        }
    }

    private void processShipmentRequest(UpdateConsoleShipmentRequest request, Set<ShipmentRequestedType> shipmentRequestedTypes) throws RunnerException { // one shipment and one/multiple console
        if(Boolean.FALSE.equals(request.isForHub())) {
            commonUtils.setInterBranchContextForColoadStation();
        }
        if(request.getConsoleIdsList() == null || request.getConsoleIdsList().isEmpty()) {
            throw new InvalidDataAccessApiUsageException("Console Ids list should not be empty!!!");
        }
        if (ShipmentRequestedType.APPROVE.equals(request.getShipmentRequestedType())) {
            try {
                consolidationService.attachShipments(request.getShipmentRequestedType(), request.getConsoleIdsList().get(0), List.of(request.getShipmentId()), false);
            } catch (RunnerException e) {
                log.error("Error while attaching shipments: {}", e.getMessage(), e);
                throw new BillingException(e.getMessage());
            }
            ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, request.getShipmentId(), "=");
            Pair<Specification<ConsoleShipmentMapping>, Pageable> pair = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
            List<ConsoleShipmentMapping> consoleShipmentMappingsForEmails = jsonHelper.convertValueToList(consoleShipmentMappingDao.findAll(pair.getLeft(), pair.getRight()).getContent(), ConsoleShipmentMapping.class);

            consoleShipmentMappingDao.deletePendingStateByShipmentId(request.getShipmentId());
            // one shipment and one console, shipment pull accepted
            // one shipment and multiple console, shipment pull and push rejected
            sendEmailsForPullRequestAccept(request.getConsoleIdsList().get(0), request.getShipmentId(), shipmentRequestedTypes, consoleShipmentMappingsForEmails);
        } else if (ShipmentRequestedType.REJECT.equals(request.getShipmentRequestedType()) || ShipmentRequestedType.WITHDRAW.equals(request.getShipmentRequestedType())) {
            // fetching from console shipment mapping
            ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, request.getShipmentId(), "=", null);
            listCommonRequest = andCriteria(CONSOLIDATION_ID, request.getConsoleIdsList(), "IN", listCommonRequest);
            Pair<Specification<ConsoleShipmentMapping>, Pageable> pair2 = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
            List<ConsoleShipmentMapping> consoleShipmentMappings = jsonHelper.convertValueToList(consoleShipmentMappingDao.findAll(pair2.getLeft(), pair2.getRight()).getContent(), ConsoleShipmentMapping.class);
            request.getConsoleIdsList().stream().forEach(consoleId -> consoleShipmentMappingDao.deletePendingStateByConsoleIdAndShipmentId(consoleId, request.getShipmentId()));
            // one shipment and multiple console, shipment pull rejected
            if(ShipmentRequestedType.REJECT.equals(request.getShipmentRequestedType())) {
                sendEmailForPullRequestReject(request.getShipmentId(), request.getConsoleIdsList(), shipmentRequestedTypes, request.getRejectRemarks(), consoleShipmentMappings);
            }
            if(ShipmentRequestedType.WITHDRAW.equals(request.getShipmentRequestedType())) {
                sendEmailForPushRequestWithdrawl(request.getShipmentId(), request.getConsoleIdsList(), shipmentRequestedTypes, request.getRejectRemarks());
            }
        }
    }


    public ResponseEntity<IRunnerResponse> shipmentRetrieveWithMeasurmentBasis(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request.getGuid() == null) {
                log.error(ShipmentConstants.SHIPMENT_ID_GUID_NULL_FOR_RETRIEVE_NTE, LoggerHelper.getRequestIdFromMDC());
                throw new RunnerException(ShipmentConstants.ID_GUID_NULL_ERROR);
            }
            UUID guid = UUID.fromString(request.getGuid());
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findByGuid(guid);
            if (!shipmentDetails.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_DETAILS_NULL_FOR_GUID_ERROR, request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            MeasurementBasisResponse response = modelMapper.map(shipmentDetails.get(), MeasurementBasisResponse.class);
            calculatePacksAndPacksUnit(shipmentDetails.get().getPackingList(), response);
            calculateContainersAndTeu(response, shipmentDetails.get().getContainersList());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private void calculateContainersAndTeu(MeasurementBasisResponse response, Set<Containers> containersList) {
        long containerCount = 0;
        Map<String, Long> containerCountMap = new HashMap<>();
        if(!CollectionUtils.isEmpty(containersList)) {
            for(Containers containers : containersList) {
                if(containers.getContainerCount() != null) {
                    containerCount = containerCount + containers.getContainerCount();
                    if(StringUtility.isNotEmpty(containers.getContainerCode())) {
                        containerCountMap.put(containers.getContainerCode(), containerCountMap.getOrDefault(containers.getContainerCode(), 0L) + containers.getContainerCount());
                    }
                }
            }

            response.setTeuCount(masterDataUtils.setContainerTeuDataWithContainers(new ArrayList<>(containersList)));
            response.setContainerData(containerCountMap);
            response.setContainerCount(containerCount);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> requestInterBranchConsole(Long shipId, Long consoleId, String remarks) throws RunnerException {
        commonUtils.setInterBranchContextForColoadStation();
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByShipmentIdAll(shipId);
        List<ConsoleShipmentMapping> pullRequests = new ArrayList<>();
        List<ConsoleShipmentMapping> pushRequests = new ArrayList<>();
        for (var consoleShip: consoleShipmentMappings) {
            ResponseEntity<IRunnerResponse> buildFailedResponse = checkAlreadyExistingConsole(consoleId, consoleShip);
            if (buildFailedResponse != null) return buildFailedResponse;
            updatePullRequests(consoleShip, pullRequests, pushRequests);
        }
        awbDao.validateAirMessaging(consoleId);
        ShipmentDetails shipmentDetails = shipmentDao.findById(shipId).orElseThrow(() -> new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE));
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(consoleId).orElseThrow(() -> new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE));
        if(checkForAirDGFlag(consolidationDetails)) {
            if(Boolean.TRUE.equals(shipmentDetails.getContainsHazardous())) {
                return ResponseHelper.buildFailedResponse(String.format(AIR_DG_SHIPMENT_NOT_ALLOWED_WITH_INTER_BRANCH_CONSOLIDATION, consolidationDetails.getConsolidationNumber()));
            }
            if(Boolean.TRUE.equals(consolidationDetails.getHazardous())) {
                return ResponseHelper.buildFailedResponse(String.format(AIR_DG_CONSOLIDATION_NOT_ALLOWED_WITH_INTER_BRANCH_SHIPMENT, shipmentDetails.getShipmentId()));
            }
        }
        ConsoleShipmentMapping entity = ConsoleShipmentMapping.builder()
                .shipmentId(shipId)
                .consolidationId(consoleId)
                .isAttachmentDone(false)
                .requestedType(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED)
                .build();

        boolean isImportShipment = false;
        if(Constants.DIRECTION_IMP.equalsIgnoreCase(shipmentDetails.getDirection())) {
            isImportShipment = true;
            consolidationService.attachShipments(ShipmentRequestedType.APPROVE, consoleId, new ArrayList<>(List.of(shipId)), false);
            var emailTemplatesRequests = commonUtils.getEmailTemplates(IMPORT_SHIPMENT_PUSH_ATTACHMENT_EMAIL);
            if(Objects.isNull(emailTemplatesRequests) || emailTemplatesRequests.isEmpty())
                return ResponseHelper.buildSuccessResponseWithWarning(TEMPLATE_NOT_FOUND_MESSAGE);
            sendImportShipmentPushAttachmentEmail(shipmentDetails, consolidationDetails, emailTemplatesRequests);
        }
        if(!isImportShipment) {
            Set<ShipmentRequestedType> shipmentRequestedTypes = new HashSet<>();
            consoleShipmentMappingDao.save(entity);
            sendEmailForNonImportShipment(shipId, consoleId, remarks, pullRequests, shipmentRequestedTypes, pushRequests);
            String warning = getWarningMsg(shipmentRequestedTypes);
            return ResponseHelper.buildSuccessResponseWithWarning(warning);
        }
        return ResponseHelper.buildSuccessResponse();
    }

    @Nullable
    private static ResponseEntity<IRunnerResponse> checkAlreadyExistingConsole(Long consoleId, ConsoleShipmentMapping consoleShip) {
        if (!Objects.equals(consoleShip.getConsolidationId(), consoleId) && Boolean.TRUE.equals(consoleShip.getIsAttachmentDone())) {
            return ResponseHelper.buildFailedResponse("These is already consolidation exist in shipment. Please detach and update shipment first.");
        }
        if (Objects.equals(consoleShip.getConsolidationId(), consoleId)) {
            return ResponseHelper.buildSuccessResponse();
        }
        return null;
    }

    private void updatePullRequests(ConsoleShipmentMapping consoleShip, List<ConsoleShipmentMapping> pullRequests, List<ConsoleShipmentMapping> pushRequests) {
        if(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED.equals(consoleShip.getRequestedType()))
            pullRequests.add(jsonHelper.convertValue(consoleShip, ConsoleShipmentMapping.class));
        if(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED.equals(consoleShip.getRequestedType()))
            pushRequests.add(jsonHelper.convertValue(consoleShip, ConsoleShipmentMapping.class));
    }

    private String getWarningMsg(Set<ShipmentRequestedType> shipmentRequestedTypes) {
        String warning = null;
        if (!shipmentRequestedTypes.isEmpty()) {
            warning = TEMPLATE_NOT_FOUND_MESSAGE;
        }
        return warning;
    }

    private void sendEmailForNonImportShipment(Long shipId, Long consoleId, String remarks, List<ConsoleShipmentMapping> pullRequests, Set<ShipmentRequestedType> shipmentRequestedTypes, List<ConsoleShipmentMapping> pushRequests) {
        if(!pullRequests.isEmpty()) {
            pullRequests.forEach(e -> consoleShipmentMappingDao.deletePendingStateByConsoleIdAndShipmentId(e.getConsolidationId(), e.getShipmentId()));
            sendEmailForPullRequestReject(shipId, pullRequests.stream().map(e -> e.getConsolidationId()).toList(), shipmentRequestedTypes,
                    remarks + "<br>Target Shipment has been requested to attach with an another consolidation already.",
                    pullRequests);
        }
        if(!pushRequests.isEmpty()) {
            pushRequests.forEach(e -> consoleShipmentMappingDao.deletePendingStateByConsoleIdAndShipmentId(e.getConsolidationId(), e.getShipmentId()));
            sendEmailForPushRequestWithdrawl(shipId, List.of(pushRequests.get(0).getConsolidationId()), shipmentRequestedTypes, remarks);
        }
        sendEmailForPushRequested(shipId, consoleId, shipmentRequestedTypes);
    }

    private ResponseEntity<IRunnerResponse> sendImportShipmentPushAttachmentEmail(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, List<EmailTemplatesRequest> emailTemplatesRequests) {
        var emailTemplateModel = emailTemplatesRequests.stream().findFirst().orElse(new EmailTemplatesRequest());

        List<String> toEmailList = new ArrayList<>();
        List<String> ccEmailsList = new ArrayList<>();
        if(consolidationDetails.getCreatedBy() != null)
            toEmailList.add(consolidationDetails.getCreatedBy());

        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        tenantIds.add(shipmentDetails.getTenantId());
        tenantIds.add(consolidationDetails.getTenantId());

        Map<String, Object> dictionary = new HashMap<>();
        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();

        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getCarriersData(Stream.of(shipmentDetails.getCarrierDetails().getShippingLine()).filter(Objects::nonNull).toList(), carrierMasterDataMap)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUnLocationsData(Stream.of(shipmentDetails.getCarrierDetails().getOriginPort(), shipmentDetails.getCarrierDetails().getDestinationPort()).filter(Objects::nonNull).toList(), unLocMap)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)), executorService);

        CompletableFuture.allOf(carrierFuture, unLocationsFuture, toAndCcEmailIdsFuture).join();

        commonUtils.getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, v1TenantSettingsMap, shipmentDetails.getTenantId(), true);
        ccEmailsList.addAll(new ArrayList<>(toEmailIds));
        ccEmailsList.addAll(new ArrayList<>(ccEmailIds));
        if(consolidationDetails.getCreatedBy() == null) {
            toEmailIds.clear();
            ccEmailIds.clear();
            commonUtils.getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, v1TenantSettingsMap, consolidationDetails.getTenantId(), false);
            toEmailList.addAll(new ArrayList<>(toEmailIds));
        }

        commonUtils.populateShipmentImportPushAttachmentTemplate(dictionary, shipmentDetails, consolidationDetails, carrierMasterDataMap, unLocMap);
        commonUtils.sendEmailNotification(dictionary, emailTemplateModel, toEmailList, ccEmailsList);
        return ResponseHelper.buildSuccessResponse();
    }

    @Override
    public ResponseEntity<IRunnerResponse> getPendingNotifications(CommonRequestModel commonRequestModel) {
        PendingNotificationRequest request = (PendingNotificationRequest) commonRequestModel.getData();
        PendingNotificationResponse<PendingShipmentActionsResponse> response = new PendingNotificationResponse<>();
        if(request.getShipmentIdList() == null || request.getShipmentIdList().isEmpty()) {
            log.info("Received empty request for pending notification in shipments", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse(response);
        }
        var notificationMap = getNotificationMap(request);
        response.setNotificationMap(notificationMap);
        return ResponseHelper.buildSuccessResponse(response);
    }


    @Override
    public ResponseEntity<IRunnerResponse> sendOceanDGApprovalEmail(OceanDGApprovalRequest dgApprovalRequest)
        throws RunnerException {
        if (Objects.isNull(dgApprovalRequest)) {
            log.error("Invalid request for sendEmailForDGApprove");
            throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
        }

        Long shipId = dgApprovalRequest.getShipmentId();
        String remarks = dgApprovalRequest.getRemarks();

        ShipmentDetails shipmentDetails = shipmentDao.findById(shipId)
            .orElseThrow(() -> new DataRetrievalFailureException("Shipment details not found for ID: " + shipId));

        if(Constants.IMP.equals(shipmentDetails.getDirection())) {
            return ResponseHelper.buildSuccessResponseWithWarning("DG approval not required for Import Shipment");
        }

        boolean isOceanDgUser = LicenseContext.isOceanDGLicense();
        OceanDGStatus dgStatus = shipmentDetails.getOceanDGStatus();
        OceanDGStatus updatedDgStatus = determineDgStatusAfterApproval(dgStatus, isOceanDgUser, shipmentDetails);
        DBOperationType operationType = determineOperationType(dgStatus, isOceanDgUser);

        boolean isShipmentdg = isOceanDG(shipmentDetails);
        log.info("DG Approval Processing: requestId={}, shipmentId={}, isOceanDgUser={}, currentStatus={}, updatedStatus={}, operationType={}, isShipmentdg={}",
            LoggerHelper.getRequestIdFromMDC(), shipId, isOceanDgUser, dgStatus, updatedDgStatus, operationType, isShipmentdg);

        String warning = null;
        if(!isShipmentdg){
            warning = "Shipment does not have any DG container or package, no need of any dg approval";
            updatedDgStatus = null;
            operationType = DG_REQUEST;
        }

        if((dgStatus == OCEAN_DG_ACCEPTED || dgStatus == OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED || dgStatus== OCEAN_DG_COMMERCIAL_REJECTED) && !checkForClass1(shipmentDetails) && warning == null){
            warning = "Shipment does not have any class1 DG container or package, no need of commercial dg approval";
            updatedDgStatus = OCEAN_DG_ACCEPTED;
            operationType = DG_APPROVE;
        }

        if(dgStatus == OCEAN_DG_COMMERCIAL_ACCEPTED){
            warning = "Shipment is already in commercial approved state";
            updatedDgStatus = OCEAN_DG_COMMERCIAL_ACCEPTED;
            operationType = COMMERCIAL_APPROVE;
        }

        if ((!isOceanDgUser || dgStatus == OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED || dgStatus== OCEAN_DG_COMMERCIAL_REJECTED) && warning == null) {
            sendEmailForApproval(shipmentDetails, remarks);
        }

        try {
            auditLogService.addAuditLog(
                AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                    .newData(OceanDGRequestLog.builder()
                        .time(LocalDateTime.now())
                        .userName(UserContext.getUser().getUsername())
                        .build())
                    .prevData(null)
                    .parent(ShipmentDetails.class.getSimpleName())
                    .parentId(shipmentDetails.getId())
                    .entityType(OceanDGRequestLog.class.getSimpleName())
                    .operation(operationType.name()).build()
            );

        }catch (Exception ex){
            log.error("Audit failed for shipmentId: {} and operation: {}. Error: {}", shipmentDetails.getId(), operationType, ex.getMessage(), ex);
        }

        shipmentDetails.setOceanDGStatus(updatedDgStatus);
        shipmentDao.save(shipmentDetails, false);

        return ResponseHelper.buildSuccessResponseWithWarning(warning);
    }

    @Override
    public ResponseEntity<IRunnerResponse> dgApprovalResponse(OceanDGRequest request)
        throws RunnerException {
        if (Objects.isNull(request)) {
            log.error("Invalid request for sendEmailForDGApprove");
            throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
        }

        ShipmentDetails shipmentDetails = shipmentDao.findById(request.getShipmentId())
            .orElseThrow(() -> new DataRetrievalFailureException("Shipment details not found for ID: " + request.getShipmentId()));

        if(Constants.IMP.equals(shipmentDetails.getDirection())) {
            return ResponseHelper.buildSuccessResponseWithWarning("DG approval not required for Import Shipment");
        }

       OceanDGStatus oldDgStatus = shipmentDetails.getOceanDGStatus();
       OceanDGStatus updatedDgStatus = getDgStatusAfterApprovalResponse(oldDgStatus, request.getStatus());

        if(updatedDgStatus == null){
            throw new RunnerException(String.format("Ocean DG status value %s is invalid", oldDgStatus));
        }

        if(StringUtils.isEmpty(request.getTaskId())){
            fetchDgUserTask(request);
        }

        String warning = sendEmailResponseToDGRequester(request, shipmentDetails, updatedDgStatus);
        DBOperationType operationType = determineOperationTypeAfterApproval(oldDgStatus, request);

        closeOceanDgTask(request);
        try {
            auditLogService.addAuditLog(
                AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                .newData(OceanDGRequestLog.builder()
                    .time(LocalDateTime.now())
                    .userName(UserContext.getUser().DisplayName)
                    .build())
                .prevData(null)
                .parent(ShipmentDetails.class.getSimpleName())
                .parentId(shipmentDetails.getId())
                .entityType(OceanDGRequestLog.class.getSimpleName())
                .operation(operationType.name()).build()
            );
        } catch (Exception ex){
            log.error("Audit failed for shipmentId: {} and operation: {}. Error: {}", shipmentDetails.getId(), operationType, ex.getMessage(), ex);
        }

        if(updatedDgStatus == OceanDGStatus.OCEAN_DG_ACCEPTED && checkForClass1(shipmentDetails)){
            updatedDgStatus = OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED;
        }
        shipmentDetails.setOceanDGStatus(updatedDgStatus);

        shipmentDao.save(shipmentDetails, false);

        return ResponseHelper.buildSuccessResponseWithWarning(warning);
    }

    @Override
    public PartiesRequest fetchOrgInfoFromV1(PartiesOrgAddressRequest request)
        throws RunnerException {
        bookingIntegrationsUtility.transformOrgAndAddressPayload(request.getParty(), request.getAddressCode(), request.getOrgCode());
        var addressDataV1 = jsonHelper.convertValue(request.getParty().getAddressData(), AddressDataV1.class);
        var orgDataV1 = jsonHelper.convertValue(request.getParty().getOrgData(), OrgDataV1.class);

        Map<String, Object> addressDataMap = jsonHelper.convertValue(addressDataV1, Map.class);
        Map<String, Object> orgDataMap = jsonHelper.convertValue(orgDataV1, Map.class);

        request.getParty().setOrgData(orgDataMap);
        request.getParty().setAddressData(addressDataMap);
        request.getParty().setOrgCode(request.getOrgCode());
        request.getParty().setAddressCode(request.getAddressCode());
        return request.getParty();
    }

    @Override
    public ResponseEntity<IRunnerResponse> hblCheck(String hblNumber, String shipmentId) {
        List<ShipmentDetailsProjection> shipmentDetails = shipmentDao.findByHblNumberAndExcludeShipmentId(hblNumber, shipmentId);

        if (ObjectUtils.isNotEmpty(shipmentDetails)) {
            String message = "The HBL provided already exists for " +
                    shipmentDetails.stream()
                            .map(sd -> "Shipment No. " + sd.getShipmentId() + " in branch " + getTenantId(sd))
                            .collect(Collectors.joining(", "));

            return ResponseHelper.buildSuccessResponse(HblCheckResponse.builder().message(message).build());
        }

        return ResponseHelper.buildSuccessResponse(HblCheckResponse.builder().build());

    }

    private String getTenantId(ShipmentDetailsProjection sd) {
        Integer tenantId = sd.getTenantId();
        try {
            return v1Service.getTenantName(List.of(tenantId)).stream().findFirst().orElse(tenantId.toString());
        } catch (Exception e) {
            return tenantId.toString();
        }
    }

    @Override
    @ExcludePermissions
    public ResponseEntity<IRunnerResponse> listWithoutTenantCheck(CommonRequestModel commonRequestModel) {
        String responseMsg;
        int totalPage = 0;
        long totalElements = 0;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(ShipmentConstants.SHIPMENT_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException(ShipmentConstants.SHIPMENT_LIST_REQUEST_NULL_ERROR);
            }
            request.setIncludeTbls(Arrays.asList(Constants.ADDITIONAL_DETAILS, Constants.CLIENT, Constants.CONSIGNER, Constants.CONSIGNEE, Constants.CARRIER_DETAILS, Constants.PICKUP_DETAILS, Constants.DELIVERY_DETAILS));
            log.info(ShipmentConstants.SHIPMENT_LIST_CRITERIA_PREPARING, LoggerHelper.getRequestIdFromMDC());
            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAllWithoutTenantFilter(tuple.getLeft(), tuple.getRight());
            log.info(ShipmentConstants.SHIPMENT_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());
            totalPage = shipmentDetailsPage.getTotalPages();
            totalElements = shipmentDetailsPage.getTotalElements();

            if(request.getIncludeColumns()==null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildListSuccessResponse(
                        convertEntityToDtoListSimplified(shipmentDetailsPage.getContent()),
                        totalPage,
                        totalElements);
            else {
                List<IRunnerResponse> filteredList =new ArrayList<>();
                for( var curr: convertEntityToDtoListSimplified(shipmentDetailsPage.getContent())){
                    RunnerPartialListResponse res=new RunnerPartialListResponse();
                    res.setData(partialFetchUtils.fetchPartialListData(curr,request.getIncludeColumns()));
                    filteredList.add( res);

                }
                return ResponseHelper.buildListSuccessResponse(
                        filteredList,
                        totalPage,
                        totalElements);
            }
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> attachDetachOrder(ShipmentOrderAttachDetachRequest shipmentRequest) {
        try {
            if(shipmentRequest.getShipmentGuid() == null)
                throw new ValidationException("Shipment GUID or Order GUID cannot be null");
            var orderList = shipmentRequest.getOrderDetailsList();
            if(orderList == null || orderList.isEmpty())
                return ResponseHelper.buildSuccessResponse();
            var shipment = shipmentDao.findByGuid(shipmentRequest.getShipmentGuid());
            if(!shipment.isPresent())
            {
                throw new ValidationException("Shipment doesn't exists!");
            }
            var shipmentId = shipment.get().getId();
            var shipmentOrders = shipmentOrderDao.findByShipmentId(shipmentId);
            Map<UUID, ShipmentOrder> shipmentOrderMap = getShipmentOrderMap(shipmentOrders);
            if(shipmentRequest.getEvent().equals("ATTACH"))
            {
                attachAndCreatedOrderWithShipment(orderList, shipmentOrderMap, shipmentId);
            }
            else if(shipmentRequest.getEvent().equals("DETACH"))
            {
                for(var order: orderList)
                {
                    if(order.getOrderGuid() != null && shipmentOrderMap.containsKey(order.getOrderGuid()))
                    {
                        shipmentOrderDao.delete(shipmentOrderMap.get(order.getOrderGuid()));
                    }
                }
            }
            else {
                throw new ValidationException("Event can only be ATTACH/DETACH");
            }
        } catch (Exception e) {
            log.error("Error while attaching/detaching order: " + e.getMessage());
            return ResponseHelper.buildFailedResponse(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
        return ResponseHelper.buildSuccessResponse();
    }

    private Map<UUID, ShipmentOrder> getShipmentOrderMap(List<ShipmentOrder> shipmentOrders) {
        Map<UUID, ShipmentOrder> shipmentOrderMap = new HashMap<>();
        if(shipmentOrders != null && !shipmentOrders.isEmpty()) {
            shipmentOrderMap = shipmentOrders.stream().filter(shipmentOrder -> shipmentOrder.getOrderGuid() != null).collect(
                    Collectors.toMap(ShipmentOrder::getOrderGuid, shipmentorder -> shipmentorder));
        }
        return shipmentOrderMap;
    }

    private void attachAndCreatedOrderWithShipment(List<ShipmentOrderAttachDetachRequest.OrderDetails> orderList, Map<UUID, ShipmentOrder> shipmentOrderMap, Long shipmentId) {
        for(var order: orderList)
        {
            if(!shipmentOrderMap.containsKey(order.getOrderGuid()) && order.getOrderGuid() != null)
            {
                shipmentOrderDao.save(
                        ShipmentOrder.builder().
                                shipmentId(shipmentId).
                                orderGuid(order.getOrderGuid()).
                                orderNumber(order.getOrderNumber()).
                                build()
                );
            }
        }
    }

    private void fetchDgUserTask(OceanDGRequest request) throws RunnerException {
        CommonV1ListRequest commonV1ListRequest = createCriteriaTaskListRequest(request.getShipmentId().toString(), SHIPMENTS_WITH_SQ_BRACKETS);
        log.info("V1 task list request: {}" , jsonHelper.convertToJson(commonV1ListRequest));

        V1DataResponse v1Response;
        try {
            v1Response = v1Service.listTask(commonV1ListRequest);
        }
        catch (Exception ex) {
            log.error("Check Task exist failed to check from V1: " + ex);
            throw new RunnerException("Check Task exist failed to check from V1: " + ex);
        }
        List<TaskCreateRequest> taskCreateRequestList = jsonHelper.convertValueToList(v1Response.getEntities(), TaskCreateRequest.class);

        if(taskCreateRequestList.isEmpty()) return;

        if(taskCreateRequestList.size() > 1){
            log.error("More than one task in Pending State of oceanDG exist for shipment : " + request.getShipmentId());
        }

        TaskCreateRequest taskCreateRequest = taskCreateRequestList.get(0);
        request.setTaskId(taskCreateRequest.getId());
        request.setUserEmail(taskCreateRequest.getUserEmail());

    }

    private void closeOceanDgTask(OceanDGRequest request){
        String remarks = request.getRemarks() == null ? "Task Rejected by DG user" :  request.getRemarks();
        TaskStatusUpdateRequest taskUpdateRequest = TaskStatusUpdateRequest.builder()
            .entityId(request.getTaskId())
            .entity(EntityDetails.builder()
                .status(request.getStatus().getValue())
                .rejectionRemarks(request.getStatus().getValue() == 2 ? remarks : null )
                .build())
            .build();


        try {
            v1Service.updateTask(taskUpdateRequest);
        }
        catch (Exception ex) {
            log.error("task updatation is failed for taskId from V1: " + taskUpdateRequest.getEntityId());
        }
    }

    private CommonV1ListRequest createCriteriaTaskListRequest(Object value1, Object value2) {
        List<Object> criteria1 = new ArrayList<>(List.of(List.of("EntityId"), "=", value1));
        List<Object> criteria2 = new ArrayList<>(List.of(List.of("EntityType"), "=", value2));
        List<Object> criteria3 = new ArrayList<>(List.of(List.of("TaskType"), "=", 22));
        List<Object> criteria4 = new ArrayList<>(List.of(List.of("Status"), "=", 0));

        return CommonV1ListRequest.builder().criteriaRequests(List.of(List.of(List.of(criteria1, "and", criteria2), "and", criteria3), "and" , criteria4)).build();
    }

    private DBOperationType determineOperationType(OceanDGStatus dgStatus, boolean isOceanDgUser) {
        if(dgStatus == OCEAN_DG_REQUESTED && isOceanDgUser) return DG_APPROVE;
        return dgStatus == OCEAN_DG_REQUESTED
            ? DBOperationType.DG_REQUEST
            : COMMERCIAL_REQUEST;
    }

    private DBOperationType determineOperationTypeAfterApproval(OceanDGStatus dgStatus, OceanDGRequest request){
        DBOperationType operationType = DG_REQUEST;
        if(dgStatus == OCEAN_DG_REQUESTED){
            if(request.getStatus() == TaskStatus.APPROVED){
                operationType = DG_APPROVE;
            }else{
                operationType = DBOperationType.DG_REJECT;
            }
        }else if(dgStatus == OCEAN_DG_COMMERCIAL_REQUESTED){
            if(request.getStatus() == TaskStatus.REJECTED){
                operationType = COMMERCIAL_APPROVE;
            }else{
                operationType = DBOperationType.COMMERCIAL_REJECT;
            }
        }
        return operationType;
    }

    private OceanDGStatus determineDgStatusAfterApproval(OceanDGStatus dgStatus, boolean isOceanDgUser, ShipmentDetails shipmentDetails) {
        boolean isClass1 = checkForClass1(shipmentDetails);
        if(dgStatus == OCEAN_DG_ACCEPTED && !isClass1){
            return dgStatus;
        }
        if ((dgStatus == OCEAN_DG_APPROVAL_REQUIRED || dgStatus == OCEAN_DG_REJECTED) && isOceanDgUser) {
            return isClass1 ? OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED : OCEAN_DG_ACCEPTED;
        } else {
            return (dgStatus == OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED || dgStatus == OCEAN_DG_COMMERCIAL_REJECTED) ? OCEAN_DG_COMMERCIAL_REQUESTED : OCEAN_DG_REQUESTED;
        }
    }


    private boolean checkForClass1(ShipmentDetails shipmentDetails) {
        if(shipmentDetails == null) return false;
        return shipmentDetails.getContainersList() != null &&
            shipmentDetails.getContainersList().stream()
                .filter(Objects::nonNull)
                .anyMatch(containers -> Boolean.TRUE.equals(containers.getHazardous()) &&
                    Optional.ofNullable(containers.getDgClass())
                        .map(dgClass -> dgClass.startsWith("1"))
                        .orElse(false))
            || shipmentDetails.getPackingList() != null &&
                shipmentDetails.getPackingList().stream()
                    .filter(Objects::nonNull)
                    .anyMatch(packing -> Boolean.TRUE.equals(packing.getHazardous()) &&
                        Optional.ofNullable(packing.getDGClass())
                            .map(dgClass -> dgClass.startsWith("1"))
                            .orElse(false));

    }

    private boolean isOceanDG(ShipmentDetails shipmentDetails) {
        if (shipmentDetails == null) return false;

        boolean containerHasDGClass = shipmentDetails.getContainersList() != null &&
            shipmentDetails.getContainersList().stream()
                .filter(Objects::nonNull)
                .anyMatch(container -> Boolean.TRUE.equals(container.getHazardous()) && container.getDgClass() != null);


        boolean packingHasDGClass = shipmentDetails.getPackingList() != null &&
            shipmentDetails.getPackingList().stream()
                .filter(Objects::nonNull)
                .anyMatch(packing -> Boolean.TRUE.equals(packing.getHazardous()) && packing.getDGClass() != null);


        return containerHasDGClass || packingHasDGClass;
    }



    private String sendEmailResponseToDGRequester(OceanDGRequest request, ShipmentDetails shipmentDetails, OceanDGStatus newStatus) throws RunnerException {

        String warningMessage = null;
        Map<OceanDGStatus, EmailTemplatesRequest> emailTemplates = new EnumMap<>(OceanDGStatus.class);
        CompletableFuture<Void> emailTemplateFuture = CompletableFuture.runAsync(
            masterDataUtils.withMdc(() -> commonUtils.getDGEmailTemplate(emailTemplates)),
            executorService
        );
        emailTemplateFuture.join();

        try {
            EmailTemplatesRequest template = emailTemplates.get(newStatus);
            if(template == null){
                warningMessage = "No template is present for status: " + newStatus;
                return warningMessage;
            }

            commonUtils.sendEmailResponseToDGRequester(template, request, shipmentDetails);
        } catch (Exception e) {
            log.error(ERROR_WHILE_SENDING_EMAIL, e.getMessage());
            warningMessage = ERROR_WHILE_SENDING_EMAIL + e.getMessage();
        }
        return warningMessage;
    }

    private void sendEmailForApproval(ShipmentDetails shipmentDetails, String remarks)
        throws RunnerException {
        OceanDGStatus oceanDGStatus = shipmentDetails.getOceanDGStatus();
        OceanDGStatus templateStatus = emailTemplateForDGApproval(oceanDGStatus);

        if(templateStatus == null){
            throw new RunnerException( String.format("User cannot send email in %s DGStatus", oceanDGStatus));
        }
        Map<OceanDGStatus, EmailTemplatesRequest> emailTemplatesRequestMap = new EnumMap<>(OceanDGStatus.class);
        VesselsResponse vesselsResponse = new VesselsResponse();
        // making v1 calls for master data
        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getDGEmailTemplate(emailTemplatesRequestMap)), executorService);
        var vesselResponseFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getVesselsData(shipmentDetails.getCarrierDetails(), vesselsResponse)), executorService);

        CompletableFuture.allOf(emailTemplateFuture, vesselResponseFuture).join();
        Integer roleId = commonUtils.getRoleId(templateStatus);
        List<String> toUserEmails = commonUtils.getUserEmailsByRoleId(roleId);
        TaskCreateResponse taskCreateResponse =  commonUtils.createTask(shipmentDetails, roleId);

        try {
            sendEmailForApproval(emailTemplatesRequestMap, toUserEmails, vesselsResponse, templateStatus, shipmentDetails, remarks,
                taskCreateResponse);
        } catch (Exception ex) {
            throw new RunnerException(ex.getMessage());
        }
    }

    private OceanDGStatus emailTemplateForDGApproval(OceanDGStatus currentStatus) {
        if (currentStatus == null || currentStatus == OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED || currentStatus == OceanDGStatus.OCEAN_DG_REJECTED ) {
            return OCEAN_DG_REQUESTED;
        } else if (currentStatus == OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED || currentStatus == OCEAN_DG_COMMERCIAL_REQUESTED || currentStatus == OceanDGStatus.OCEAN_DG_COMMERCIAL_REJECTED) {
            return OCEAN_DG_COMMERCIAL_REQUESTED;
        } else {
            return null;
        }
    }

    private OceanDGStatus getDgStatusAfterApprovalResponse(OceanDGStatus currentStatus, TaskStatus approvalStatus) {
        if (currentStatus == OCEAN_DG_COMMERCIAL_REQUESTED) {
            return approvalStatus == TaskStatus.APPROVED ?
                OCEAN_DG_COMMERCIAL_ACCEPTED :
                OceanDGStatus.OCEAN_DG_COMMERCIAL_REJECTED;
        } else if (currentStatus == OCEAN_DG_REQUESTED) {
            return approvalStatus == TaskStatus.APPROVED  ?
                OceanDGStatus.OCEAN_DG_ACCEPTED :
                OceanDGStatus.OCEAN_DG_REJECTED;
        }
        return null;
    }

    private Map<Long, List<PendingShipmentActionsResponse>> getNotificationMap(PendingNotificationRequest request) {
        // Get data of all consolidation pulling this shipment that are not yet attached
        var pullRequestedEnum = ShipmentRequestedType.SHIPMENT_PULL_REQUESTED;
        Map<Long, List<PendingShipmentActionsResponse>> notificationResultMap = new HashMap<>();

        if(commonUtils.getCurrentTenantSettings() == null || !Boolean.TRUE.equals(commonUtils.getCurrentTenantSettings().getIsMAWBColoadingEnabled())) {
            return notificationResultMap;
        }

        try {
            ListCommonRequest listRequest = constructListCommonRequest("shipmentId", request.getShipmentIdList(), "IN");
            listRequest = andCriteria("requestedType", pullRequestedEnum.name(), "=", listRequest);
            listRequest = andCriteria("isAttachmentDone", false, "=", listRequest);
            Pair<Specification<ConsoleShipmentMapping>, Pageable> consoleShipMappingPair = fetchData(listRequest, ConsoleShipmentMapping.class);
            Page<ConsoleShipmentMapping> mappingPage = consoleShipmentMappingDao.findAll(consoleShipMappingPair.getLeft(), consoleShipMappingPair.getRight());

            List<Long> consolidationIds = mappingPage.getContent().stream().map(ConsoleShipmentMapping::getConsolidationId).toList();
            final var consoleShipmentsMap = mappingPage.getContent().stream().collect(Collectors.toMap(
                ConsoleShipmentMapping::getConsolidationId, Function.identity(), (oldVal, newVal) -> oldVal)
            );

            commonUtils.setInterBranchContextForColoadStation();

            listRequest = constructListCommonRequest("id", consolidationIds, "IN");
            listRequest.setContainsText(request.getContainsText());
            listRequest.setSortRequest(request.getSortRequest());
            Pair<Specification<ConsolidationDetails>, Pageable> pair = fetchData(listRequest, ConsolidationDetails.class, ConsolidationService.tableNames);
            Page<ConsolidationDetails> consolPage = consolidationDetailsDao.findAll(pair.getLeft(), pair.getRight());

            var tenantIdList = new HashSet<String>();
            var locCodeList =  new HashSet<String>();
            final CarrierDetails nullCarrierDetails = new CarrierDetails();
            consolPage.getContent().stream().forEach(i -> {
                tenantIdList.add(StringUtility.convertToString(i.getTenantId()));
                var carrierDetails = Optional.ofNullable(i.getCarrierDetails()).orElse(nullCarrierDetails);
                locCodeList.add(carrierDetails.getOriginPort());
                locCodeList.add(carrierDetails.getDestinationPort());
            });
            Map<String, TenantModel> v1TenantData = masterDataUtils.fetchInTenantsList(tenantIdList);
            Map<String, EntityTransferUnLocations> v1LocationData = masterDataUtils.fetchInBulkUnlocations(locCodeList, EntityTransferConstants.LOCATION_SERVICE_GUID);

            masterDataUtils.pushToCache(v1TenantData, CacheConstants.TENANTS, tenantIdList, new TenantModel(), null);
            masterDataUtils.pushToCache(v1LocationData, CacheConstants.UNLOCATIONS, locCodeList, new EntityTransferUnLocations(), null);

            // console id vs list of ship ids
            Map<Long, List<Long>> consolVsShipIdMap = new HashMap<>();

            // generate mapping for shipment id vs list of pulling consol(s)
            for(var mapping : mappingPage.getContent()) {
                if(!notificationResultMap.containsKey(mapping.getShipmentId())) {
                    notificationResultMap.put(mapping.getShipmentId(), new ArrayList<>());
                }
                if(!consolVsShipIdMap.containsKey(mapping.getConsolidationId())) {
                    consolVsShipIdMap.put(mapping.getConsolidationId(), new ArrayList<>());
                }
                consolVsShipIdMap.get(mapping.getConsolidationId()).add(mapping.getShipmentId());
            }

            consolPage.getContent().stream().forEach(i -> {
                var res = mapToNotification(i, consoleShipmentsMap, v1TenantData, v1LocationData);
                consolVsShipIdMap.get(i.getId()).forEach(shipId -> notificationResultMap.get(shipId).add(res));
            });

        }
        catch(Exception e) {
            log.error("Error while generating notification map for input Shipment", LoggerHelper.getRequestIdFromMDC(), e.getMessage());
        }

        return notificationResultMap;
    }

    private PendingShipmentActionsResponse mapToNotification(ConsolidationDetails consol, Map<Long, ConsoleShipmentMapping> consoleShipmentsMap, Map<String, TenantModel> v1TenantData, Map<String, EntityTransferUnLocations> v1LocationData) {
        var carrierDetails = Optional.ofNullable(consol.getCarrierDetails()).orElse(new CarrierDetails());
        var tenantData = Optional.ofNullable(v1TenantData.get(StringUtility.convertToString(consol.getTenantId()))).orElse(new TenantModel());
        return PendingShipmentActionsResponse.builder()
            .consolId(consol.getId())
            .consolidationNumber(consol.getReferenceNumber())
            .masterBill(consol.getMawb())
            .ata(carrierDetails.getAta())
            .atd(carrierDetails.getAtd())
            .eta(carrierDetails.getEta())
            .etd(carrierDetails.getEtd())
            .pol(Optional.ofNullable(v1LocationData.get(carrierDetails.getOriginPort())).map(EntityTransferUnLocations::getLookupDesc).orElse(carrierDetails.getOriginPort()))
            .pod(Optional.ofNullable(v1LocationData.get(carrierDetails.getDestinationPort())).map(EntityTransferUnLocations::getLookupDesc).orElse(carrierDetails.getDestinationPort()))
            .lat(consol.getLatDate())
            .branch(tenantData.getCode() + " - " + tenantData.getTenantName())
            .branchDisplayName(tenantData.displayName)
            .hazardous(consol.getHazardous())
            .requestedBy(consoleShipmentsMap.get(consol.getId()).getCreatedBy())
            .requestedOn(consoleShipmentsMap.get(consol.getId()).getCreatedAt())
            .build();
    }

    @Override
    public ResponseEntity<IRunnerResponse> calculateShipmentSummary(CommonRequestModel commonRequestModel) throws RunnerException {
        CalculateShipmentSummaryRequest request = (CalculateShipmentSummaryRequest) commonRequestModel.getData();

        var response = CalculateShipmentSummaryResponse.builder().build();
        commonUtils.setInterBranchContextForHub();

        if(request.getShipmentIdList() == null || request.getShipmentIdList().isEmpty()) {
            return ResponseHelper.buildSuccessResponse(response);
        }
        List<ShipmentDetails> shipmentDetailsList = shipmentDao.findShipmentsByIds(request.getShipmentIdList().stream().collect(Collectors.toSet()));

        double totalWeight = 0;
        double totalVolume = 0;
        double chargeableWeight = 0;
        int totalPacks = 0;
        String packsUnit = null;

        String toWeightUnit = Constants.WEIGHT_UNIT_KG;
        String toVolumeUnit = Constants.VOLUME_UNIT_M3;
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        toWeightUnit = getToWeightUnit(shipmentSettingsDetails, toWeightUnit);
        toVolumeUnit = getToVolumeUnit(shipmentSettingsDetails, toVolumeUnit);

        String transportMode = "";

        for (var ship: shipmentDetailsList) {
            double winDef = UnitConversionUtility.convertUnit(Constants.MASS, ship.getWeight(), ship.getWeightUnit(), toWeightUnit).doubleValue();
            double volDef = UnitConversionUtility.convertUnit(VOLUME, ship.getVolume(), ship.getVolumeUnit(), toVolumeUnit).doubleValue();
            totalWeight = totalWeight + winDef;
            totalVolume = totalVolume + volDef;
            chargeableWeight = chargeableWeight + (ship.getChargable() != null ? ship.getChargable().doubleValue(): 0);

            if(!isStringNullOrEmpty(ship.getPacksUnit())) {
                if(packsUnit == null)
                    packsUnit = ship.getPacksUnit();
                else if(!packsUnit.equals(ship.getPacksUnit()))
                    packsUnit = MPK;
            }
            if(ship.getNoOfPacks() != null)
                totalPacks = totalPacks + ship.getNoOfPacks();
            transportMode = ship.getTransportMode();
        }
        response.setTotalPacksWithUnit(totalPacks + " " + (packsUnit != null? packsUnit : ""));
        response.setTotalPacksWeight(String.format(Constants.STRING_FORMAT, IReport.convertToWeightNumberFormat(BigDecimal.valueOf(totalWeight), v1TenantSettingsResponse), toWeightUnit));
        response.setTotalPacksVolume(String.format(Constants.STRING_FORMAT, IReport.convertToVolumeNumberFormat(BigDecimal.valueOf(totalVolume), v1TenantSettingsResponse), toVolumeUnit));

        if(Objects.equals(transportMode, Constants.TRANSPORT_MODE_AIR)) {
            chargeableWeight = CommonUtils.roundOffAirShipment(chargeableWeight);
        }
        chargeableWeight = BigDecimal.valueOf(chargeableWeight).setScale(2, RoundingMode.HALF_UP).doubleValue();
        response.setPacksChargeableWeight(String.format(Constants.STRING_FORMAT, IReport.convertToWeightNumberFormat(BigDecimal.valueOf(chargeableWeight), v1TenantSettingsResponse), toWeightUnit));
        return ResponseHelper.buildSuccessResponse(response);
    }

    private String getToVolumeUnit(ShipmentSettingsDetails shipmentSettingsDetails, String toVolumeUnit) {
        if(!isStringNullOrEmpty(shipmentSettingsDetails.getVolumeChargeableUnit()))
            toVolumeUnit = shipmentSettingsDetails.getVolumeChargeableUnit();
        return toVolumeUnit;
    }

    private String getToWeightUnit(ShipmentSettingsDetails shipmentSettingsDetails, String toWeightUnit) {
        if(!isStringNullOrEmpty(shipmentSettingsDetails.getWeightChargeableUnit()))
            toWeightUnit = shipmentSettingsDetails.getWeightChargeableUnit();
        return toWeightUnit;
    }

    public void sendEmailForApproval(Map<OceanDGStatus, EmailTemplatesRequest> emailTemplatesRequestMap, List<String> toEmailIds,
        VesselsResponse vesselsResponse, OceanDGStatus templateStatus, ShipmentDetails shipmentDetails, String remarks,
        TaskCreateResponse taskCreateResponse) throws RunnerException {
        EmailTemplatesRequest emailTemplate = Optional.ofNullable(emailTemplatesRequestMap.get(templateStatus))
            .orElseThrow(() -> new RunnerException("template is not present for : " + templateStatus));

        if (CollectionUtils.isEmpty(toEmailIds)) {
            throw new RunnerException("There are no DG certified users for your branch. Please contact admin");
        }

        Map<String, Object> dictionary = new HashMap<>();
        populateDictionary(templateStatus, dictionary, shipmentDetails, vesselsResponse, remarks, taskCreateResponse);

        emailTemplate.setBody(generateEmailBody(dictionary, shipmentDetails, emailTemplate.getBody()));
        notificationService.sendEmail(emailTemplate.getBody(), emailTemplate.getSubject(), new ArrayList<>(toEmailIds), new ArrayList<>());
    }

    private void populateDictionary(OceanDGStatus templateStatus, Map<String, Object> dictionary,
        ShipmentDetails shipmentDetails,
        VesselsResponse vesselsResponse, String remarks, TaskCreateResponse taskCreateResponse) {

        if (templateStatus == OCEAN_DG_REQUESTED) {
            commonUtils.populateDictionaryForOceanDGApproval(dictionary, shipmentDetails, vesselsResponse, remarks, taskCreateResponse);
        } else if (templateStatus == OCEAN_DG_COMMERCIAL_REQUESTED) {
            commonUtils.populateDictionaryForOceanDGCommercialApproval(dictionary, shipmentDetails, vesselsResponse, remarks, taskCreateResponse);
        }
    }

    private String populateTableWithData(String tableTemplate, ShipmentDetails shipmentDetails) {
        if(tableTemplate.isEmpty()) return tableTemplate;

        Document document = Jsoup.parse(tableTemplate);
        Element table = document.select("table").first();

        assert table != null;
        Element rowTemplate = table.select("tbody tr").get(1);

        rowTemplate.remove();
        Map<Long, String> containerIdNumberMap = Optional.ofNullable(shipmentDetails)
            .map(ShipmentDetails::getContainersList)
            .orElse(Collections.emptySet())
            .stream()
            .filter(container -> container.getId() != null && container.getContainerNumber() != null)
            .collect(Collectors.toMap(Containers::getId, Containers::getContainerNumber));


        processTableWithPackingList(shipmentDetails, rowTemplate, containerIdNumberMap, table);

        processTableWithContainerList(shipmentDetails, rowTemplate, table);

        return table.outerHtml();
    }

    private void processTableWithContainerList(ShipmentDetails shipmentDetails, Element rowTemplate, Element table) {
        for (Containers containers : shipmentDetails.getContainersList()) {
            if(!Boolean.TRUE.equals(containers.getHazardous())) continue;

            Element newRow = rowTemplate.clone();
            newRow.select("td").get(0).text(
                    getValueOrDefault(containers.getPacks(), "") + " " + getValueOrDefault(containers.getPacksType(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(1).text(
                    getValueOrDefault(containers.getContainerNumber(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(2).text(
                    getValueOrDefault(containers.getDgClass(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(3).text(
                    getValueOrDefault(containers.getUnNumber(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(4).text(
                    getValueOrDefault(containers.getProperShippingName(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(5).text(
                    getValueOrDefault(containers.getPackingGroup(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(6).text(
                    getValueOrDefault(containers.getMinimumFlashPoint(), "") +
                            getValueOrDefault(containers.getMinimumFlashPointUnit(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(7).text(
                Boolean.TRUE.equals(containers.getMarinePollutant()) ? "Yes" : "No"
            ).attr(STYLE, PADDING_10_PX);


            table.select("tbody").first().appendChild(newRow);
        }
    }

    public static <T> T getValueOrDefault(T value, T defaultValue) {
        return value != null ? value : defaultValue;
    }

    private void processTableWithPackingList(ShipmentDetails shipmentDetails, Element rowTemplate, Map<Long, String> containerIdNumberMap, Element table) {
        for (Packing packing : shipmentDetails.getPackingList()) {
            if(!Boolean.TRUE.equals(packing.getHazardous())) continue;

            Element newRow = rowTemplate.clone();
            newRow.select("td").get(0).text(
                    getValueOrDefault(packing.getPacks(), "") + " " + getValueOrDefault(packing.getPacksType(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(1).text(
                containerIdNumberMap.get(packing.getContainerId()) != null
                    ? String.valueOf(containerIdNumberMap.get(packing.getContainerId()))
                    : ""
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(2).text(
                    getValueOrDefault(packing.getDGClass(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(3).text(
                    getValueOrDefault(packing.getUnNumber(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(4).text(
                    getValueOrDefault(packing.getProperShippingName(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(5).text(
                    getValueOrDefault(packing.getPackingGroup(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(6).text(

                (packing.getMinimumFlashPoint() != null ? packing.getMinimumFlashPoint() : "") +
                    (packing.getMinimumFlashPointUnit() != null ? packing.getMinimumFlashPointUnit() : "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(7).text(
                Boolean.TRUE.equals(packing.getMarinePollutant()) ? "Yes" : "No"
            ).attr(STYLE, PADDING_10_PX);


            table.select("tbody").first().appendChild(newRow);
        }
    }

    private String extractTableTemplate(String htmlTemplate) {
        int tableStartIndex = htmlTemplate.indexOf("<table");
        int tableEndIndex = htmlTemplate.indexOf("</table>") + "</table>".length();

        if (tableStartIndex != -1 && tableEndIndex > tableStartIndex) {
            return htmlTemplate.substring(tableStartIndex, tableEndIndex);
        }

        return "";
    }

    private String generateEmailBody(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, String htmlTemplate) {
        String tableTemplate = extractTableTemplate(htmlTemplate);

        String populatedTable = populateTableWithData(tableTemplate, shipmentDetails);

        String emailBody = htmlTemplate.replace(tableTemplate, populatedTable);
        emailBody = commonUtils.replaceTagsFromData(dictionary, emailBody);
        return emailBody;
    }

    public ShipmentDetailsResponse createFromBookingServiceAPI(CommonRequestModel commonRequestModel)
    {
        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Shipment Create From Booking with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        ShipmentDetails shipmentDetails = jsonHelper.convertValue(request, ShipmentDetails.class);
        try {
            if(request.getConsolidationList() != null)
                shipmentDetails.setConsolidationList(new HashSet<>(jsonHelper.convertValueToList(request.getConsolidationList().stream().toList(), ConsolidationDetails.class)));
            if(request.getContainersList() != null)
                shipmentDetails.setContainersList(new HashSet<>(jsonHelper.convertValueToList(request.getContainersList().stream().toList(), Containers.class)));
            populateOriginDestinationAgentDetailsForBookingShipment(shipmentDetails);
            shipmentDetails = getShipment(shipmentDetails);
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            if(shipmentSettingsDetails.getAutoEventCreate() != null && shipmentSettingsDetails.getAutoEventCreate())
                autoGenerateCreateEvent(shipmentDetails);
            autoGenerateEvents(shipmentDetails);
            createAutomatedEvents(shipmentDetails, EventConstants.BKCR, commonUtils.getUserZoneTime(LocalDateTime.now()), null);
            Long shipmentId = shipmentDetails.getId();
            List<Packing> updatedPackings = getAndSetPackings(request, shipmentId, shipmentDetails);
            List<RoutingsRequest> routingsRequest = request.getRoutingsList();
            if (ObjectUtils.isNotEmpty(routingsRequest))
                shipmentDetails.setRoutingsList(routingsDao.saveEntityFromShipment(jsonHelper.convertValueToList(routingsRequest, Routings.class), shipmentId));

            List<ReferenceNumbersRequest> referenceNumbersRequest = request.getReferenceNumbersList();
            if (ObjectUtils.isNotEmpty(referenceNumbersRequest))
                shipmentDetails.setReferenceNumbersList(referenceNumbersDao.saveEntityFromShipment(jsonHelper.convertValueToList(referenceNumbersRequest, ReferenceNumbers.class), shipmentId));

            List<ShipmentOrderRequest> shipmentOrderRequestList = request.getShipmentOrders();
            if(ObjectUtils.isNotEmpty(shipmentOrderRequestList)) {
                shipmentDetails.setShipmentOrders(shipmentOrderDao.updateEntityFromShipment(jsonHelper.convertValueToList(shipmentOrderRequestList, ShipmentOrder.class), shipmentId));
            }

            checkAllContainerAssigned(shipmentDetails, updatedPackings);

            List<NotesRequest> notesRequest = getNotesRequests(request, shipmentId);

            dependentServiceHelper.pushShipmentDataToDependentService(shipmentDetails, true, false, null);

            setShipmentFromBooking(shipmentDetails, notesRequest);
            createAuditLog(shipmentDetails, null, DBOperationType.CREATE.name());
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ValidationException(e.getMessage());
        }
        return jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);
    }

    private void checkAllContainerAssigned(ShipmentDetails shipmentDetails, List<Packing> updatedPackings) {
        if(shipmentDetails.getContainersList() != null && !shipmentDetails.getContainersList().isEmpty()) {
            hblService.checkAllContainerAssigned(shipmentDetails, shipmentDetails.getContainersList(), updatedPackings);
        }
    }

    private List<Packing> getAndSetPackings(ShipmentRequest request, Long shipmentId, ShipmentDetails shipmentDetails) {
        List<Packing> updatedPackings = new ArrayList<>();
        if (request.getPackingList() != null) {
            updatedPackings = packingDao.saveEntityFromShipment(jsonHelper.convertValueToList(request.getPackingList(), Packing.class), shipmentId);
            shipmentDetails.setPackingList(updatedPackings);
        }
        return updatedPackings;
    }

    public void setShipmentFromBooking(ShipmentDetails shipmentDetails, List<NotesRequest> notesRequest){
        try {
            shipmentDetails.setNotesList(null);
            shipmentSync.syncFromBooking(shipmentDetails, null, notesRequest);
        } catch (Exception e){
            log.error(SyncingConstants.ERROR_SYNCING_SHIPMENTS, e);
        }

    }


    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> cancel(CommonRequestModel commonRequestModel) throws RunnerException {
        CommonGetRequest commonGetRequest = (CommonGetRequest) commonRequestModel.getData();

        Optional<ShipmentDetails> shipmentOptional = shipmentDao.findById(commonGetRequest.getId());
        if (shipmentOptional.isEmpty()) {
            throw new RunnerException(DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG);
        }

        ShipmentDetails shipment = shipmentOptional.get();

        // update shipment status by calling a dao method
        shipment.setStatus(ShipmentStatus.Cancelled.getValue());
        shipmentDao.update(shipment, false);

        // Delete the shipment pending pull/push request tasks when the shipment got cancelled
        if (Boolean.TRUE.equals(commonUtils.getCurrentTenantSettings().getIsMAWBColoadingEnabled())) {
            log.info("Request: {} | Deleting console_shipment_mapping due to shipment cancelled for shipment: {}", LoggerHelper.getRequestIdFromMDC(), shipment.getShipmentId());
            consoleShipmentMappingDao.deletePendingStateByShipmentId(shipment.getId());
        }
        dependentServiceHelper.pushShipmentDataToDependentService(shipment, false, false, shipment.getContainersList());
        syncShipment(shipment, null, null, null, null, false);
        if (commonUtils.getCurrentTenantSettings().getP100Branch() != null && commonUtils.getCurrentTenantSettings().getP100Branch())
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> bookingIntegrationsUtility.updateBookingInPlatform(shipment)), executorService);

        return ResponseHelper.buildSuccessResponse();
    }

    public void populateOriginDestinationAgentDetailsForBookingShipment(ShipmentDetails shipmentDetails) {
        ConsolidationDetails consolidationDetails = null;
        if(!CommonUtils.setIsNullOrEmpty(shipmentDetails.getConsolidationList())) {
            consolidationDetails = shipmentDetails.getConsolidationList().iterator().next();
        }
        if(consolidationDetails != null && !Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole())) {
            boolean consolUpdated = false;
            if (CommonUtils.checkPartyNotNull(consolidationDetails.getSendingAgent())) {
                setExportBrokerForInterBranchConsole(shipmentDetails, consolidationDetails);
            } else if (shipmentDetails.getAdditionalDetails() != null && CommonUtils.checkPartyNotNull(shipmentDetails.getAdditionalDetails().getExportBroker())) {
                consolidationDetails.setSendingAgent(shipmentDetails.getAdditionalDetails().getExportBroker());
                consolUpdated = true;
            }

            if (CommonUtils.checkPartyNotNull(consolidationDetails.getReceivingAgent())) {
                setImportBrokerForInterBranchConsole(shipmentDetails, consolidationDetails);
            } else if (shipmentDetails.getAdditionalDetails() != null && CommonUtils.checkPartyNotNull(shipmentDetails.getAdditionalDetails().getImportBroker())) {
                consolidationDetails.setReceivingAgent(shipmentDetails.getAdditionalDetails().getImportBroker());
                consolUpdated = true;
            }

            if (consolUpdated) {
                consolidationDetailsDao.save(consolidationDetails, false);
            }
        }
        if (consolidationDetails == null) {
            populateImportExportBrokerForShipment(shipmentDetails);
        }
    }

    private void populateImportExportBrokerForShipment(ShipmentDetails shipmentDetails) {
        if (Constants.DIRECTION_EXP.equals(shipmentDetails.getDirection()) &&
                (shipmentDetails.getAdditionalDetails() == null || !CommonUtils.checkPartyNotNull(shipmentDetails.getAdditionalDetails().getExportBroker()))) {
            if(shipmentDetails.getAdditionalDetails() == null) {
                shipmentDetails.setAdditionalDetails(new AdditionalDetails());
            }
            shipmentDetails.getAdditionalDetails().setExportBroker(v1ServiceUtil.getDefaultAgentOrgParty(null));
        } else if (Constants.DIRECTION_IMP.equals(shipmentDetails.getDirection()) &&
                (shipmentDetails.getAdditionalDetails() == null || !CommonUtils.checkPartyNotNull(shipmentDetails.getAdditionalDetails().getImportBroker()))) {
            if(shipmentDetails.getAdditionalDetails() == null) {
                shipmentDetails.setAdditionalDetails(new AdditionalDetails());
            }
            shipmentDetails.getAdditionalDetails().setImportBroker(v1ServiceUtil.getDefaultAgentOrgParty(null));
        }
    }

    private void setImportBrokerForInterBranchConsole(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails) {
        if(shipmentDetails.getAdditionalDetails() != null &&
                !CommonUtils.checkSameParties(consolidationDetails.getReceivingAgent(), shipmentDetails.getAdditionalDetails().getImportBroker())) {
            shipmentDetails.getAdditionalDetails().setImportBroker(commonUtils.removeIdFromParty(consolidationDetails.getReceivingAgent()));
        } else if (shipmentDetails.getAdditionalDetails() == null) {
            shipmentDetails.setAdditionalDetails(new AdditionalDetails());
            shipmentDetails.getAdditionalDetails().setImportBroker(commonUtils.removeIdFromParty(consolidationDetails.getReceivingAgent()));
        }
    }

    private void setExportBrokerForInterBranchConsole(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails) {
        if(shipmentDetails.getAdditionalDetails() != null &&
                !CommonUtils.checkSameParties(consolidationDetails.getSendingAgent(), shipmentDetails.getAdditionalDetails().getExportBroker())) {
            shipmentDetails.getAdditionalDetails().setExportBroker(commonUtils.removeIdFromParty(consolidationDetails.getSendingAgent()));
        } else if (shipmentDetails.getAdditionalDetails() == null) {
            shipmentDetails.setAdditionalDetails(new AdditionalDetails());
            shipmentDetails.getAdditionalDetails().setExportBroker(commonUtils.removeIdFromParty(consolidationDetails.getSendingAgent()));
        }
    }

    @Override
    @Transactional
    @SuppressWarnings("java:S4144") //Supressing this identical method because it will be updated in v3
    public ResponseEntity<IRunnerResponse> createV3(CommonRequestModel commonRequestModel) {
        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();
        this.setColoadingStation(request);
        ShipmentDetailsResponse shipmentDetailsResponse = this.createShipment(request, false, false);

        return ResponseHelper.buildSuccessResponse(shipmentDetailsResponse);
    }

    @Override
    @Transactional
    @SuppressWarnings("java:S4144") //Supressing this identical method because it will be updated in v3
    public ResponseEntity<IRunnerResponse> completeUpdateV3(CommonRequestModel commonRequestModel) throws RunnerException {
        ShipmentRequest shipmentRequest = (ShipmentRequest) commonRequestModel.getData();
        this.setColoadingStation(shipmentRequest);
        ShipmentDetailsResponse response = completeUpdateShipment(shipmentRequest, false);
        return ResponseHelper.buildSuccessResponse(response);
    }

    @Override
    @SuppressWarnings("java:S4144") //Supressing this identical method because it will be updated in v3
    public ResponseEntity<IRunnerResponse> retrieveByIdV3(CommonRequestModel commonRequestModel, boolean getMasterData) {
        String responseMsg;
        try {
            return ResponseHelper.buildSuccessResponse(retireveShipmentData(commonRequestModel, false, false));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    @SuppressWarnings("java:S4144") //Supressing this identical method because it will be updated in v3
    public ResponseEntity<IRunnerResponse> fullShipmentsListV3(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            // LATER- implement actual logic with filters
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(ShipmentConstants.SHIPMENT_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException(ShipmentConstants.SHIPMENT_LIST_REQUEST_NULL_ERROR);
            }
            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = this.findAllWithOutIncludeColumn(tuple.getLeft(), tuple.getRight());
            log.info(ShipmentConstants.SHIPMENT_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());
            if(request.getIncludeColumns()==null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildListSuccessResponse(
                        convertEntityListToFullShipmentList(shipmentDetailsPage.getContent()),
                        shipmentDetailsPage.getTotalPages(),
                        shipmentDetailsPage.getTotalElements());
            else {
                List<IRunnerResponse>filteredList=new ArrayList<>();
                for( var curr: convertEntityListToFullShipmentList(shipmentDetailsPage.getContent())){
                    RunnerPartialListResponse res=new RunnerPartialListResponse();
                    res.setData(partialFetchUtils.fetchPartialListData(curr,request.getIncludeColumns()));
                    filteredList.add( res);

                }
                return ResponseHelper.buildListSuccessResponse(
                        filteredList,
                        shipmentDetailsPage.getTotalPages(),
                        shipmentDetailsPage.getTotalElements());
            }
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    @SuppressWarnings("java:S4144") //Supressing this identical method because it will be updated in v3
    public ResponseEntity<IRunnerResponse> listV3(CommonRequestModel commonRequestModel, boolean getMasterData) {
        String responseMsg;
        int totalPage = 0;
        long totalElements = 0;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(ShipmentConstants.SHIPMENT_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException(ShipmentConstants.SHIPMENT_LIST_REQUEST_NULL_ERROR);
            }
            if(Boolean.TRUE.equals(request.getNotificationFlag())) {
                Page<Long> eligibleShipmentId = shipmentDao.getIdWithPendingActions(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED,
                        PageRequest.of(Math.max(0,request.getPageNo()-1), request.getPageSize()));

                List<Long> shipmentIds = notificationDao.findEntityIdsByEntityType(SHIPMENT);

                Set<Long> uniqueShipmentIds = new HashSet<>(eligibleShipmentId.getContent());
                uniqueShipmentIds.addAll(shipmentIds);

                List<Long> combinedShipmentIds = new ArrayList<>(uniqueShipmentIds);

                andCriteria("id", combinedShipmentIds, "IN", request);

                totalElements = combinedShipmentIds.size();
                int pageSize = request.getPageSize();
                totalPage = (int) ((totalElements + pageSize - 1) / pageSize);
            }
            checkWayBillNumberCriteria(request);
            log.info(ShipmentConstants.SHIPMENT_LIST_CRITERIA_PREPARING, LoggerHelper.getRequestIdFromMDC());
            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());

            log.info(ShipmentConstants.SHIPMENT_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());
            if(!Boolean.TRUE.equals(request.getNotificationFlag())) {
                totalPage = shipmentDetailsPage.getTotalPages();
                totalElements = shipmentDetailsPage.getTotalElements();
            }
            if(request.getIncludeColumns()==null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildListSuccessResponse(
                        convertEntityListToDtoList(shipmentDetailsPage.getContent(), getMasterData),
                        totalPage,
                        totalElements);
            else {
                List<IRunnerResponse>filteredList=new ArrayList<>();
                for( var curr: convertEntityListToDtoList(shipmentDetailsPage.getContent(), getMasterData)){
                    RunnerPartialListResponse res=new RunnerPartialListResponse();
                    res.setData(partialFetchUtils.fetchPartialListData(curr,request.getIncludeColumns()));
                    filteredList.add(res);
                }
                return ResponseHelper.buildListSuccessResponse(
                        filteredList,
                        totalPage,
                        totalElements);
            }
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }

    private void checkPermissionsForCloning(ShipmentDetails shipmentDetails) {
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        Boolean countryAirCargoSecurity = shipmentSettingsDetails.getCountryAirCargoSecurity();
        if (Boolean.TRUE.equals(countryAirCargoSecurity)) {
            if (!CommonUtils.checkAirSecurityForShipment(shipmentDetails)) {
                throw new ValidationException(Constants.AIR_SECURITY_PERMISSION_MSG);
            }
        } else {
            if (checkForDGShipmentAndAirDgFlag(shipmentDetails) && !isAirDgUser())
                throw new ValidationException("You do not have necessary permissions for this.");
        }
    }


}
