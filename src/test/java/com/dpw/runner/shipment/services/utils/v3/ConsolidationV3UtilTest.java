package com.dpw.runner.shipment.services.utils.v3;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.nimbusds.jose.util.Pair;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
public class ConsolidationV3UtilTest {

  @Mock
  private CommonUtils commonUtils;

  @Mock
  private IConsoleShipmentMappingDao consoleShipmentMappingDao;

  @Mock
  private IShipmentDao shipmentDao;

  @Mock
  private ConsolidationDetails consolidationDetails;

  @InjectMocks
  private ConsolidationV3Util consolidationV3Util;

  @BeforeEach
  void setUp() {
    consolidationDetails = new ConsolidationDetails();
  }

  @Test
  void testCheckConsolidationEligibleForCFSValidation_AllCriteriaMet_ReturnsTrue() {
    // Arrange
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
    consolidationDetails.setShipmentType(Constants.DIRECTION_EXP);
    ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
    shipmentSettingsDetails.setEnableLclConsolidation(true);
    lenient().when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);
    // Act
    boolean result = consolidationV3Util.checkConsolidationEligibleForCFSValidation(consolidationDetails);

    // Assert
    assertTrue(result);
  }

  @Test
  void testCheckConsolidationEligibleForCFSValidation_NonSeaTransportMode_ReturnsFalse() {
    // Arrange
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
    consolidationDetails.setShipmentType(Constants.DIRECTION_EXP);
    ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
    shipmentSettingsDetails.setEnableLclConsolidation(false);
    lenient().when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);
    // Act
    boolean result = consolidationV3Util.checkConsolidationEligibleForCFSValidation(consolidationDetails);

    // Assert
    assertFalse(result);
  }

  @Test
  void testCheckConsolidationEligibleForCFSValidation_NullTransportMode_ReturnsFalse() {
    // Arrange
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
    consolidationDetails.setShipmentType(Constants.IMP);
    // Act
    boolean result = consolidationV3Util.checkConsolidationEligibleForCFSValidation(consolidationDetails);

    // Assert
    assertFalse(result);
  }

  @Test
  void testCheckConsolidationEligibleForCFSValidation_NonExportShipmentType_ReturnsFalse() {
    // Arrange
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
    // Act
    boolean result = consolidationV3Util.checkConsolidationEligibleForCFSValidation(consolidationDetails);

    // Assert
    assertFalse(result);
  }

  @Test
  void getShipmentListTest1(){
    ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
    consoleShipmentMapping.setShipmentId(1L);

    when(consoleShipmentMappingDao.findByConsolidationId(anyLong()))
        .thenReturn(List.of(consoleShipmentMapping));
    ShipmentDetails shipmentDetails = new ShipmentDetails();
    PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(List.of(shipmentDetails));

    ListCommonRequest listReq = constructListCommonRequest("id", 1, "=");
    Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listReq, ShipmentDetails.class);

    when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);

    List<ShipmentDetails> result = consolidationV3Util.getShipmentsList(1L);
    assertNotNull(result);
  }
}
