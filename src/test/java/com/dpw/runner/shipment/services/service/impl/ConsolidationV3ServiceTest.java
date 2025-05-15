package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.impl.BillingServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotificationDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IReferenceNumbersDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITruckDriverDetailsDao;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.dpw.runner.shipment.services.service.interfaces.IEventsV3Service;
import com.dpw.runner.shipment.services.service.interfaces.ILogsHistoryService;
import com.dpw.runner.shipment.services.service.interfaces.INetworkTransferService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingsSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.BookingIntegrationsUtility;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.ConsolidationV3Util;
import com.dpw.runner.shipment.services.utils.ConsolidationValidationV3Util;
import com.dpw.runner.shipment.services.utils.GetNextNumberHelper;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.NetworkTransferV3Util;
import com.dpw.runner.shipment.services.utils.ProductIdentifierUtility;
import java.util.concurrent.ExecutorService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.CacheManager;
import org.springframework.test.util.ReflectionTestUtils;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
public class ConsolidationV3ServiceTest {
  @Mock
  private ExecutorService executorService;

  @Mock
  private CacheManager cacheManager;

  @Mock
  private CustomKeyGenerator keyGenerator;

  @Mock
  private IConsolidationDetailsDao consolidationDetailsDao;

  @Mock
  private IConsoleShipmentMappingDao consoleShipmentMappingDao;

  @Mock
  private IPartiesDao partiesDao;

  @Mock
  private IPackingsSync packingsADSync;

  @Mock
  private ConsolidationValidationV3Util consolidationValidationV3Util;

  @Mock
  private ConsolidationV3Util consolidationV3Util;

  @Mock
  private JsonHelper jsonHelper;

  @Mock
  private KafkaProducer producer;

  @Mock
  private ITrackingServiceAdapter trackingServiceAdapter;

  @Mock
  private ILogsHistoryService logsHistoryService;

  @Mock
  private BillingServiceAdapter billingServiceAdapter;

  @Mock
  private IPackingDao packingDao;

  @Mock
  private IPackingV3Service packingV3Service;

  @Mock
  private IEventDao eventDao;

  @Mock
  private IRoutingsV3Service routingsV3Service;

  @Mock
  private IEventsV3Service eventV3Service;

  @Mock
  private IShipmentServiceV3 shipmentV3Service;

  @Mock
  private IContainerDao containerDao;

  @Mock
  private IContainerV3Service containerV3Service;

  @Mock
  private IContainerService containerService;

  @Mock
  private IPackingService packingService;

  @Mock
  private CommonUtils commonUtils;

  @Mock
  private IShipmentDao shipmentDao;

  @Mock
  private IShipmentsContainersMappingDao shipmentsContainersMappingDao;

  @Mock
  private INotificationDao notificationDao;

  @Mock
  private IShipmentSync shipmentSync;

  @Mock
  private IConsolidationSync consolidationSync;

  @Mock
  private MasterDataUtils masterDataUtils;

  @Mock
  private MasterDataKeyUtils masterDataKeyUtils;

  @Mock
  private IAwbDao awbDao;

  @Mock
  private INetworkTransferService networkTransferService;

  @Mock
  private INetworkTransferDao networkTransferDao;

  @Mock
  private IEventService eventService;

  @Mock
  private V1ServiceUtil v1ServiceUtil;

  @Mock
  private IAuditLogService auditLogService;

  @Mock
  private IReferenceNumbersDao referenceNumbersDao;

  @Mock
  private ITruckDriverDetailsDao truckDriverDetailsDao;

  @Mock
  private DependentServiceHelper dependentServiceHelper;

  @Mock
  private BookingIntegrationsUtility bookingIntegrationsUtility;

  @Mock
  private NetworkTransferV3Util networkTransferV3Util;

  @Mock
  private IShipmentSettingsDao shipmentSettingsDao;

  @Mock
  private IV1Service v1Service;

  @Mock
  private ProductIdentifierUtility productEngine;

  @Mock
  private GetNextNumberHelper getNextNumberHelper;

  @Mock
  private ModelMapper modelMapper;

  @Mock
  @Qualifier("executorServiceMasterData")
  private ExecutorService executorServiceMasterData;

  @InjectMocks
  private ConsolidationV3Service consolidationV3Service;

  @BeforeEach
  void setUp() {
    // mock @Value properties manually
    ReflectionTestUtils.setField(consolidationV3Service, "senderQueue", "test-queue");
    ReflectionTestUtils.setField(consolidationV3Service, "includeMasterData", true);
  }


}
