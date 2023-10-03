//package com.dpw.runner.shipment.services.ReportingService.Reports;
//
//import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
//import com.dpw.runner.shipment.services.entity.ShipmentDetails;
//
//import java.util.Map;
//
//public class ShipmentHouseBlReport implements IReport{
//    @Override
//    public Map<String, Object> getData(Long id) {
//        return null;
//    }
//
//    @Override
//    IDocumentModel getDocumentModel(Long id) {
//        return null;
//    }
//
//    @Override
//    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
//        var qc_data = new SeawayBillModel();
//        var user = (UserDefinition)Authorization.UserDefinition;
//        string DeliveryEmailAddress = string.Empty;
//        List<BillChargesRow> chargesRows = new List<BillChargesRow>();
//        AddressRepository addressRepository = new AddressRepository();
//        AddressRow deliveryToAddress = null;
//        AddressRow deliveryAgentAddress = null;
//        AddressRow deliveryTransportCompany = null;
//        AddressRow deliveryCfs = null;
//        AddressRow pickupTransportCompany = null;
//        AddressRow pickupFrom = null;
//        AddressRow pickupCfs = null;
//        ConsolidationRow consolRow = null;
//        List<PackingRow> packsDetails = new List<PackingRow>();
//        List<String> consignee = null;
//        List<String> consigner = null;
//        Dictionary<String, int> containerCountGrouped = new Dictionary<String, int>();
//        Dictionary<String, int> containerPacksGrouped = new Dictionary<String, int>();
//        Dictionary<String, Double> containerWeightGrouped = new Dictionary<String, Double>();
//        Dictionary<String, Double> containerVolumeGrouped = new Dictionary<String, Double>();
//        List<CommonContainersRow> allContainersList = new List<CommonContainersRow>();
//        var noofPackages = 0;
//        string serviceModeDesc = null;
//        var billRow = new BillRow();
//        int decimalPlaces = 2;
//        string goods_CO = null;
//        string paymentTerms = null;
//        string releaseType = null;
//        string issuePlaceCountryName = null;
//        PickupRow pickupResponse = null;
//        List<string> bookingCarriageVessalVoyage = new List<string>();
//        List<string> bookingPreCarriageMode = new List<string>();
//        using (var connection = SqlConnections.NewFor<ShipmentsRow>())
//        {
//            var o = ShipmentsRow.Fields;
//            var IncludeColumns = new List<string>
//            {
//
//            };
//            var includehashset = new HashSet<string>(IncludeColumns);
//            var retreq = new RetrieveRequest
//            {
//                IncludeColumns = includehashset,
//                        ColumnSelection = RetrieveColumnSelection.Details,
//            };
//
//
//            retreq.EntityId = this.ShipmentId;
//
//            var repo = new ShipmentsRepository();
//            var res = repo.Retrieve(connection, retreq);
//            qc_data.shipment = res.Entity;
//
//            var tsres = getTenantSettings(connection, user.TenantId);
//            decimalPlaces = tsres.DecimalPlaces == null ? 2 : (int)tsres.DecimalPlaces;
//            if (isHBL == true) {
//                qc_data.blobject = GetBlObject(this.ShipmentId);
//            }
//
//            if (qc_data.shipment.ShipmentContainers == null || qc_data.shipment.ShipmentContainers.Count == 0)
//            {
//                Dictionary<String,BlObjectContainersRow> blConatinerMap = new Dictionary<string, BlObjectContainersRow>();
//                if (isHBL == true)
//                {
//                    List<BlObjectContainersRow> ContainerList = ReportHelper.getContainersFromBLObjectId(qc_data.blobject.Id, connection);
//                    if (ContainerList != null)
//                    {
//                        foreach (var item in ContainerList)
//                        {
//                            if(!string.IsNullOrEmpty(item.ContainerNumber)){
//                                blConatinerMap.Add(item.ContainerNumber,item);
//                            }
//                        }
//                    }
//                }
//                List<ShipmentContainersRow> containerList = new List<ShipmentContainersRow>();
//                List<CommonContainersRow> commonContainerList = ReportHelper.getContainersFromShipmentId(this.ShipmentId, connection);
//                allContainersList = commonContainerList;
//                if (commonContainerList != null)
//                {
//                    foreach (var item in commonContainerList)
//                    {
//                        ShipmentContainersRow shipmentContainersRow = ReportHelper.getShipmentContainer(item, decimalPlaces);
//                        if(!string.IsNullOrEmpty(shipmentContainersRow.ContainerNumber) && blConatinerMap.ContainsKey(shipmentContainersRow.ContainerNumber)){
//                            BlObjectContainersRow blRow = blConatinerMap[shipmentContainersRow.ContainerNumber];
//                            shipmentContainersRow.BL_ContainerType = blRow.ContainerType;
//                            shipmentContainersRow.BL_SealNumber = blRow.SealNumber;
//                            shipmentContainersRow.BL_GrossWeight = Math.Round(blRow.ContainerGrossWeight.GetValueOrDefault(), decimalPlaces);
//                            shipmentContainersRow.BL_GrossWeightUnit = blRow.ContainerGrossWeightUnit;
//                            shipmentContainersRow.BL_GrossVolume = Math.Round(blRow.ContainerGrossVolume.GetValueOrDefault(), decimalPlaces).ToString();
//                            shipmentContainersRow.BL_GrossVolumeUnit = blRow.ContainerGrossVolumeUnit;
//                            shipmentContainersRow.BL_NoofPackages = blRow.NoOfPackages;
//                            shipmentContainersRow.BL_CarrierSealNumber = blRow.CarrierSealNumber;
//                            shipmentContainersRow.BL_CustomSealNumber = item.CustomsSealNumber;
//                        }
//                        shipmentContainersRow.ShipmentContainerPacks = item.Packs;
//                        shipmentContainersRow.ShipmentContainerPacksType = item.PacksType;
//
//                        int value = 0;
//                        if (item.ContainerTypeCode!=null)
//                            containerCountGrouped[item.ContainerTypeCode] =  containerCountGrouped.TryGetValue(item.ContainerTypeCode, out value) ? value + Convert.ToInt32(item.ContainerCount) : Convert.ToInt32(item.ContainerCount);
//                        if (item.PacksType!=null)
//                            containerPacksGrouped[item.PacksType] = containerPacksGrouped.TryGetValue(item.PacksType, out value) ? value + Convert.ToInt32(item.Packs) : Convert.ToInt32(item.Packs);
//                        if (item.GrossWeightUnit!=null)
//                            containerWeightGrouped[item.GrossWeightUnit] = containerWeightGrouped.ContainsKey(item.GrossWeightUnit) ? containerWeightGrouped.Get(item.GrossWeightUnit) + Convert.ToDouble(item.GrossWeight) : Convert.ToDouble(item.GrossWeight);
//                        if (item.GrossVolumeUnit!=null)
//                            containerVolumeGrouped[item.GrossVolumeUnit] = item.GrossVolumeUnit!=null && containerVolumeGrouped.ContainsKey(item.GrossVolumeUnit) ? containerVolumeGrouped.Get(item.GrossVolumeUnit) + Convert.ToDouble(item.GrossVolume) : Convert.ToDouble(item.GrossVolume);
//                        if(item.NoofPackages != null){
//                            noofPackages =  noofPackages + Convert.ToInt32(item.NoofPackages);
//                        }
//
//                        // adding the goods description after concatenating all the descriptions from the packing if the flag is on
//                        if(tsres.MultipleShipmentEnabled.HasValue && qc_data.shipment.ContainerAutoWeightVolumeUpdate.HasValue && tsres.MultipleShipmentEnabled.Value && qc_data.shipment.ContainerAutoWeightVolumeUpdate.Value){
//                            List<PackingRow> packRows = new PackingRepository().List(connection, new ListRequest(){
//                                Criteria = (new Criteria("ShipmentId") == this.ShipmentId)
//                            })?.Entities;
//                            if(packRows != null && packRows.Count > 0){
//                                foreach(PackingRow packRow in packRows){
//                                    if(!string.IsNullOrEmpty(packRow.ContainerNumber) && shipmentContainersRow.ContainerNumber == packRow.ContainerNumber && !string.IsNullOrEmpty(packRow.HandlingInfo)){
//                                        if(shipmentContainersRow.HandlingInfo != "")
//                                            shipmentContainersRow.HandlingInfo = shipmentContainersRow.HandlingInfo + "\n";
//                                        shipmentContainersRow.HandlingInfo = shipmentContainersRow.HandlingInfo + packRow.HandlingInfo;
//                                    }
//                                }
//                            }
//                        }else{
//                            shipmentContainersRow.HandlingInfo = item.HandlingInfo;
//                        }
//                        containerList.Add(shipmentContainersRow);
//                    }
//                }
//                qc_data.shipment.ShipmentContainers = containerList;
//            }
//
//            if (qc_data.shipment.ConsolidationIds != null && qc_data.shipment.ConsolidationIds.Count > 0)
//            {
//                qc_data.consolidation = populateConsolidation(qc_data.shipment.ConsolidationIds[0], connection);
//            }
//
//            var crepo = new TenantsRepository();
//            retreq.EntityId = user.TenantId;
//            var cres = crepo.Retrieve(connection, retreq);
//            qc_data.shipment = res.Entity;
//            qc_data.tenant = cres.Entity;
//
//            consolRow = getFirstConsolidation(user.TenantId, connection);
//
//            var fld = PackingRow.Fields;
//            var packsRepo = new PackingRepository();
//            var listrequest = new ListRequest
//            {
//                Criteria = (new Criteria("ShipmentId") == this.ShipmentId)
//            };
//            try
//            {
//                using (var uow = new UnitOfWork(connection))
//                {
//                    packsDetails = packsRepo.List(uow.Connection, listrequest).Entities;
//                }
//            }
//            catch (Exception)
//            {
//                packsDetails = null;
//            }
//
//            var masterListRepo = new MasterListsRepository();
//            var flds = MasterListsRow.Fields;
//            var listreq = new ListRequest
//            {
//                IncludeColumns = includehashset,
//                        // Criteria = (new Criteria("TenantId") == user.TenantId &&
//                        // new Criteria("ItemValue") == qc_data.shipment.ReleaseType
//                        // && new Criteria("ItemType") == MasterListTypes.ReleaseType
//                        Criteria = (new Criteria("ItemValue") == qc_data.shipment.ReleaseType
//                                && new Criteria("ItemType") == MasterListTypes.ReleaseType
//
//                        )
//            };
//
//            try
//            {
//                MasterListsRow masterListRes = (MasterListsRow)masterListRepo.List(connection, listreq).Entities.FirstOrDefault();
//                qc_data.releasetype = masterListRes.ItemDescription;
//            }
//            catch (Exception)
//            {
//                qc_data.releasetype = null;
//            }
//
//            if (qc_data.shipment.VesselName != null)
//            {
//                // retreq.EntityId = qc_data.shipment.VesselName;
//
//                // var vrepo = new VesselsRepository();
//                // var vres = vrepo.Retrieve(connection, retreq);
//                qc_data.vesselName = qc_data.shipment.VesselName;
//            }
//            try
//            {
//                var billrepo = new BillRepository();
//                var listreqcriteria = new ListRequest
//                {
//                    Criteria = (new Criteria("EntityId") == qc_data.shipment.Id.ToString() & new Criteria("EntityType") == "SHIPMENT")
//                };
//                qc_data.firstbill = billrepo.List(connection, listreqcriteria).Entities;
//            }
//            catch (Exception)
//            {
//                qc_data.firstbill = null;
//            }
//            if (qc_data.firstbill != null && qc_data.firstbill.Count > 0)
//            {
//                billRow = qc_data.firstbill[0];
//                foreach (var bill in qc_data.firstbill)
//                {
//                    var billChargesRepo = new BillChargesRepository();
//                    var listRequest = new ListRequest();
//                    listRequest.Criteria = new Criteria("BillId") == bill.Guid.ToString();
//                    List<BillChargesRow> rows = billChargesRepo.List(connection, listRequest).Entities;
//                    if (rows != null || rows.Count == 0)
//                    {
//                        foreach (BillChargesRow charge in rows)
//                        {
//                            charge.isPrepaidCharge = charge.PaymentType == "PPD" ? true : false;
//                            chargesRows.Add(charge);
//                        }
//                    }
//
//                }
//            }
//
//
//
//            qc_data.IsMultipleCopy = true;
//            if (res.Entity.Originals == null)
//            {
//                res.Entity.Originals = 1;
//            }
//            if (res.Entity.CopyBills == null)
//            {
//                res.Entity.CopyBills = 0;
//            }
//
//            qc_data.originalcount = res.Entity.Originals;
//            qc_data.copycount = res.Entity.CopyBills;
//            serviceModeDesc = getMasterListData(connection, MasterListTypes.ServiceModes, qc_data.shipment.ServiceMode)?.ItemDescription;
//
//            DeliveryEmailAddress = EmailAddressFromDeliveryAddressID(qc_data.shipment.DeliveryToAddressId, connection);
//            if (string.IsNullOrWhiteSpace(DeliveryEmailAddress))
//            {
//                DeliveryEmailAddress = GetEmailFromDeliveryTo(qc_data.shipment.DeliveryToId, connection);
//            }
//            RetrieveRequest retReq = new RetrieveRequest();
//            if (qc_data.shipment.DeliveryToAddressId != null)
//            {
//                retReq.EntityId = qc_data.shipment.DeliveryToAddressId;
//                deliveryToAddress = addressRepository.Retrieve(connection, retReq).Entity;
//            }
//            if (qc_data.shipment.DeliveryAgentAddressId != null)
//            {
//                retReq.EntityId = qc_data.shipment.DeliveryAgentAddressId;
//                deliveryAgentAddress = addressRepository.Retrieve(connection, retReq).Entity;
//            }
//
//            EventsRow eventsRow = filterEventForShippingOnBoard((int) qc_data.shipment.Id, (int)qc_data.tenant.TenantId, "[Shipments]", connection);
//            if (eventsRow != null)
//            {
//                qc_data.shipeedOnBoard = eventsRow.Actual;
//            }
//
//            if (qc_data.shipment.DeliveryTransportCopantAddressId != null)
//            {
//                retReq.EntityId = qc_data.shipment.DeliveryTransportCopantAddressId;
//                deliveryTransportCompany = addressRepository.Retrieve(connection, retReq).Entity;
//            }
//            if (qc_data.shipment.dCfsAddressId != null)
//            {
//                retReq.EntityId = qc_data.shipment.dCfsAddressId;
//                deliveryCfs = addressRepository.Retrieve(connection, retReq).Entity;
//            }
//            if (qc_data.shipment.PickupFromAddressId != null)
//            {
//                retReq.EntityId = qc_data.shipment.PickupFromAddressId;
//                pickupFrom = addressRepository.Retrieve(connection, retReq).Entity;
//            }
//            if (qc_data.shipment.PickTransportCopantAddressId != null)
//            {
//                retReq.EntityId = qc_data.shipment.PickTransportCopantAddressId;
//                pickupTransportCompany = addressRepository.Retrieve(connection, retReq).Entity;
//            }
//            if (qc_data.shipment.CfsAddressId != null)
//            {
//                retReq.EntityId = qc_data.shipment.CfsAddressId;
//                pickupCfs = addressRepository.Retrieve(connection, retReq).Entity;
//            }
//            goods_CO = getMasterListData(connection, MasterListTypes.Countries, qc_data?.shipment?.CountryOfOriginofGoods)?.ItemDescription;
//            paymentTerms = getMasterListData(connection, MasterListTypes.Payment, qc_data?.shipment?.PaymentTerms)?.ItemDescription;
//            releaseType = getMasterListData(connection, MasterListTypes.ReleaseType, qc_data?.shipment?.ReleaseType)?.ItemDescription;
//            issuePlaceCountryName = getMasterListData(connection, MasterListTypes.Countries, qc_data?.shipment?.IssuePlaceCountry)?.ItemDescription;
//
//            if (qc_data.shipment != null && qc_data.shipment.BookingCarriages != null && qc_data.shipment.BookingCarriages.Count > 0)
//            {
//                List<string> CarriageModesToCheckInMaster = new List<string>();
//                foreach (var bookingCarriageRow in qc_data.shipment.BookingCarriages)
//                {
//                    if (bookingCarriageRow.CarriageType == ReportConstants.PRE_CARRIAGE)
//                    {
//                        CarriageModesToCheckInMaster.Add(bookingCarriageRow.CarriageMode);
//                        bookingCarriageVessalVoyage.Add(bookingCarriageRow.Vessel);
//                        bookingCarriageVessalVoyage.Add(bookingCarriageRow.Voyage);
//                    }
//                }
//
//                var masterListRequest = new ListRequest();
//                masterListRequest.Criteria = (new Criteria("ItemType") == MasterListTypes.CarriageMode && new Criteria("ItemValue").In(CarriageModesToCheckInMaster));
//                ListResponse<MasterListsRow> masterListsBookingCarriageRows = new ListResponse<MasterListsRow>();
//                masterListsBookingCarriageRows = getMasterListDataInBulk(connection, masterListRequest);
//
//                if (masterListsBookingCarriageRows.Entities != null && masterListsBookingCarriageRows.Entities.Count > 0)
//                {
//                    foreach (var masterListsBookingCarriageRow in masterListsBookingCarriageRows.Entities)
//                    {
//                        bookingPreCarriageMode.Add(masterListsBookingCarriageRow.ItemDescription);
//                    }
//                }
//            }
//
//            pickupResponse = GetPickUpForShipment(connection);
//        }
//
//        qc_data.noofpackages_word = GetNextNumberHelper.NumberToWords(qc_data.shipment.Packs);
//        qc_data.userdisplayname = user.DisplayName;
//        //get the vessel name
//
//        string vesselName = GetTheVesselName(qc_data.shipment.VesselId);
//        if (!string.IsNullOrEmpty(vesselName))
//        {
//            qc_data.vesselName = vesselName;
//        }
//        var serializeShipment = JsonConvert.SerializeObject(qc_data.shipment);
//        var dictionary = JsonConvert.DeserializeObject<Dictionary<string, object>>(serializeShipment);
//        PrintUtil.JsonDateFormat(ref dictionary);
//        if (qc_data.blobject != null)
//        {
//            var serializeQuote = JsonConvert.SerializeObject(qc_data.blobject);
//            var blObjectdictionary = (JsonConvert.DeserializeObject<Dictionary<string, object>>(serializeQuote));
//            PrintUtil.JsonDateFormat(ref blObjectdictionary);
//            foreach ( KeyValuePair<string, object> kvp in blObjectdictionary )
//            {
//                if(dictionary.ContainsKey(kvp.Key)){
//                    dictionary.Remove(kvp.Key);
//                }
//                dictionary.Add(kvp.Key,kvp.Value);
//            }
//        }
//        dictionary["NoOfPackages"] = noofPackages;
//        dictionary["ContainerCountGrouped"] = ReportHelper.concatGroupedContainerCount(containerCountGrouped);
//        dictionary["ContainerPacksGrouped"] = ReportHelper.concatGroupedContainerCount(containerPacksGrouped);
//        dictionary["ContainerWeightWithXSeparated"] = ReportHelper.concatGroupedFieldValues(containerWeightGrouped, decimalPlaces);
//        dictionary["ContainerVolumeWithXSeparated"] = ReportHelper.concatGroupedFieldValues(containerVolumeGrouped, decimalPlaces);
//        dictionary["ContainerWeightGrouped"] = ReportHelper.concatGroupedFields(containerWeightGrouped, decimalPlaces);
//        dictionary["ContainerVolumeGrouped"] = ReportHelper.concatGroupedFields(containerVolumeGrouped, decimalPlaces);
//        dictionary["DeliveryAgent"] = null;
//        dictionary[ReportConstants.CONTAINER_COUNT_BY_CODE] = ReportHelper.getCountByContainerTypeCode(allContainersList);
//        ShipmentDetails
//        if (qc_data.shipment != null && qc_data.shipment.FreightLocal != null)
//        {
//            dictionary[Texts.Forms.Membership.DocumentKeys.FreightLocal] = qc_data.shipment.FreightLocal;
//        }
//        if (qc_data.shipment != null && !String.IsNullOrEmpty(qc_data.shipment.FreightLocalCurrency))
//        {
//            dictionary[Texts.Forms.Membership.DocumentKeys.FreightLocalCurrency] = qc_data.shipment.FreightLocalCurrency;
//        }
//        if (qc_data.shipment != null && qc_data.shipment.FreightOverseas != null)
//        {
//            dictionary[Texts.Forms.Membership.DocumentKeys.FreightOverseas] = ReportHelper.addCommas(qc_data.shipment.FreightOverseas);
//        }
//        if (qc_data.shipment != null && String.IsNullOrEmpty(qc_data.shipment.FreightOverseasCurrency ) )
//        {
//            dictionary[Texts.Forms.Membership.DocumentKeys.FreightOverseasCurrency] = qc_data.shipment.FreightOverseasCurrency;
//        }
//        if (qc_data.shipment != null && qc_data.shipment.Id != null)
//        {
//            ListRequest listRequest = new ListRequest();
//            listRequest.Criteria = new Criteria("ShipmentId") == qc_data.shipment.Id.Value;
//            ConsolidationAddressRepository consolidationAddressRepository = new ConsolidationAddressRepository();
//            using (var connection = SqlConnections.NewFor<ConsolidationRow>())
//            {
//                List<ConsolidationAddressRow> consolidationAddressRowList = consolidationAddressRepository.List(connection, listRequest).Entities;
//                MasterListsRepository masterListsRepository = new MasterListsRepository();
//                if (consolidationAddressRowList != null && consolidationAddressRowList.Count > 0)
//                {
//                    foreach (ConsolidationAddressRow consolidationAddressRow in consolidationAddressRowList)
//                    {
//                        if (consolidationAddressRow.Type == Texts.Forms.Membership.DocumentKeys.CustomHouseAgent)
//                        {
//                            OrganizationsRepository organizationsRepository = new OrganizationsRepository();
//                            OrganizationsRow organizationsRow = new OrganizationsRow();
//                            if (consolidationAddressRow.OrgId != null)
//                            {
//                                listRequest.Criteria = new Criteria("Id") == consolidationAddressRow.OrgId.Value;
//                                organizationsRow = organizationsRepository.List(connection, listRequest).Entities.FirstOrDefault();
//                                if (organizationsRow != null && !String.IsNullOrEmpty(organizationsRow.FullName))
//                                {
//                                    dictionary[Texts.Forms.Membership.DocumentKeys.CHAPartyDescription] = organizationsRow.FullName;
//                                }
//                            }
//                        }
//                    }
//                }
//            }
//        }
//        using (var connection = SqlConnections.NewFor<ShipmentsRow>())
//        {
//            var tenSett = getTenantSettings(connection, user.TenantId);
//            if (tenSett != null && tenSett.EnableIGMDetails != null && tenSett.EnableIGMDetails.Value == true){
//                if (qc_data.shipment != null && qc_data.shipment.Custom_ShipType!=null &&  qc_data.shipment.Custom_ShipType.ToUpper() == "IMP"){
//
//                    if (qc_data.shipment.IGMFileDate != null && qc_data.shipment.IGMFileDate.Value != null){
//                        dictionary["IGMFileDate"] = qc_data.shipment.IGMFileDate.Value ;
//                    }
//                    if (qc_data.shipment.IGMFileNo != null && qc_data.shipment.IGMFileNo!= null){
//                        dictionary["IGMFileNo"] = qc_data.shipment.IGMFileNo;
//                    }
//                    if (qc_data.shipment.IGMInwardDate != null && qc_data.shipment.IGMInwardDate.Value != null){
//                        dictionary["IGMInwardDate"] = qc_data.shipment.IGMInwardDate.Value ;
//                    }
//                    if (qc_data.shipment.InwardDateandTime != null && qc_data.shipment.InwardDateandTime.Value != null){
//                        dictionary["InwardDateandTime"] = qc_data.shipment.InwardDateandTime.Value ;
//                    }
//                    if (qc_data.shipment.LineNumber != null){
//                        dictionary["LineNumber"] = qc_data.shipment.LineNumber.Value ;
//                    }
//                    if (qc_data.shipment.SubLineNumber != null ){
//                        dictionary["SubLineNumber"] = qc_data.shipment.SubLineNumber.Value ;
//                    }
//
//                    if (qc_data.shipment.IsInland != null && qc_data.shipment.IsInland.Value == true){
//                        dictionary ["IsInland"] = qc_data.shipment.IsInland.Value == true ? "Yes" : "No" ;
//
//                        if (qc_data.shipment.SMTPIGMDate != null && qc_data.shipment.SMTPIGMDate.Value != null){
//                            dictionary["SMTPIGMDate"] = qc_data.shipment.SMTPIGMDate.Value ;
//                        }
//
//                        if (qc_data.shipment.SMTPIGMNumber != null && qc_data.shipment.SMTPIGMNumber != null){
//                            dictionary["SMTPIGMNumber"] = qc_data.shipment.SMTPIGMNumber ;
//                        }
//
//                        if (qc_data.shipment.LocalLineNumber != null ){
//                            dictionary["LocalLineNumber"] = qc_data.shipment.LocalLineNumber.Value ;
//                        }
//
//
//                    }
//                }
//            }
//        }
//        if (qc_data.consolidation != null)
//        {
//            dictionary["ReceivingAgentName"] = qc_data.consolidation.ReceivingAgentName;
//            var receivingAgent = getOrgAddress(qc_data.consolidation.ReceivingAgentName, qc_data.consolidation.ReceivinAgentAddress1, qc_data.consolidation.ReceivinAgentAddress2, getCityCountry(qc_data.consolidation.ReceivingAgentCity, qc_data.consolidation.ReceivingAgentCountry), qc_data.consolidation.ReceivingAgentEmail, qc_data.consolidation.ReceivingAgentContactPhone);
//            dictionary["ReceivingAgentAddress"] = receivingAgent;
//            if(receivingAgent.Count>0)
//                dictionary["DeliveryAgent"] = receivingAgent;
//            dictionary["SendingAgentName"] = qc_data.consolidation.SendingAgentName;
//            var sendingAgent = getOrgAddress(null, qc_data.consolidation.SendingAgentAddress1, qc_data.consolidation.SendingAgentAddress2, getCityCountry(qc_data.consolidation.SendingAgentCity, qc_data.consolidation.SendingAgentCountry), qc_data.consolidation.SendingAgentEmail, qc_data.consolidation.SendingAgentContactPhone);
//            dictionary["SendingAgentAddress"] = sendingAgent;
//            dictionary["AgentReference"]=  qc_data.consolidation.AgentReference;
//            dictionary["ConsolNumber"] = qc_data.consolidation.ConsolidationNumber;
//
//            if (qc_data.consolidation.IsSendingAgentFreeTextAddress.Value)
//            {
//                dictionary["SendingAgentAddressFreeText"] = ReportHelper.getAddressList(qc_data.consolidation.SendingAgentFreeTextAddress);
//            }
//            else
//            {
//                dictionary["SendingAgentAddressFreeText"] = dictionary["SendingAgentAddress"];
//            }
//
//            if (qc_data.consolidation.IsReceivingAgentFreeTextAddress.Value)
//            {
//                dictionary["ReceivingAgentAddressFreeText"] = ReportHelper.getAddressList(qc_data.consolidation.ReceivingAgentFreeTextAddress);
//            }
//            else
//            {
//                dictionary["ReceivingAgentAddressFreeText"] = dictionary["ReceivingAgentAddress"];
//            }
//
//        } else {
//            var receivingAgent = getOrgAddress(null, qc_data.shipment.ReceivinAgentAddress1, qc_data.shipment.ReceivinAgentAddress2, getCityCountry(qc_data.shipment.ReceivingAgentCity, qc_data.shipment.ReceivingAgentCountry), qc_data.shipment.ReceivingAgentEmail, qc_data.shipment.ReceivingAgentContactPhone);
//            dictionary["ReceivingAgentAddress"] = receivingAgent;
//            var sendingAgent = getOrgAddress(null, qc_data.shipment.SendingAgentAddress1, qc_data.shipment.SendingAgentAddress2, getCityCountry(qc_data.shipment.SendingAgentCity, qc_data.shipment.SendingAgentCountry), qc_data.shipment.SendingAgentEmail, qc_data.shipment.SendingAgentContactPhone);
//            dictionary["SendingAgentAddress"] = sendingAgent;
//            dictionary["AgentReference"] = qc_data.shipment.AgentReference;
//        }
//        ReferenceNumbersRow referenceNumbersRow = qc_data.shipment.ReferenceNumbers.FirstOrDefault(e => (e.Type.Equals("ERN")));
//        if (referenceNumbersRow == null && qc_data.consolidation != null)
//        {
//            referenceNumbersRow = qc_data.consolidation.ReferenceNumbers.FirstOrDefault(e => (e.Type.Equals("ERN")));
//        }
//        if (referenceNumbersRow != null)
//        {
//            dictionary["ExportReferenceNumber"] = referenceNumbersRow.ReferenceNumber;
//        }
//        referenceNumbersRow = qc_data.shipment.ReferenceNumbers.FirstOrDefault(e => (e.Type.Equals("CAN")));
//        if (referenceNumbersRow != null)
//        {
//            dictionary["CANNumber"] = referenceNumbersRow.ReferenceNumber;
//        }
//        dictionary["BillRemarks"] = billRow != null ? billRow.Remarks : "";
//        List<BillChargesRow> originalChargesRows = new List<BillChargesRow>();
//        List<BillChargesRow> copyChargesRows = new List<BillChargesRow>();
//        dictionary["asAgreed"] = false;
//        dictionary["copyAsAgreed"] = false;
//
//        if (qc_data.shipment.ChargesApply == "AGR")
//        {
//            dictionary["asAgreed"] = true;
//            dictionary["copyAsAgreed"] = true;
//        }
//        else if (qc_data.shipment.ChargesApply == "CPP" || qc_data.shipment.ChargesApply == "CAL" || qc_data.shipment.ChargesApply == "CCL")
//        {
//            dictionary["asAgreed"] = true;
//        }
//
//        if (qc_data.shipment.ChargesApply == "NON" || qc_data.shipment.ChargesApply == null)
//        {
//            dictionary["hasCharges"] = false;
//        }
//        else
//        {
//            dictionary["hasCharges"] = true;
//            getChargeRows(ref originalChargesRows, ref copyChargesRows, chargesRows, qc_data.shipment.ChargesApply);
//        }
//        dictionary["charges"] = originalChargesRows;
//        if(originalChargesRows != null && originalChargesRows.Count > 0)
//        {
//            string json = JsonConvert.SerializeObject(originalChargesRows);
//            var values = JsonConvert.DeserializeObject<List<Dictionary<string, object>>>(json);
//            values.ForEach(v =>
//                    {
//            if(v["OverseasSellAmount"] != null){
//                v["OverseasSellAmount"] = ReportHelper.addCommas(v["OverseasSellAmount"].ToString());
//            }
//                });
//            dictionary["charges"] = values;
//        }
//        dictionary["copyCharges"] = copyChargesRows;
//
//        var notifyPartyAddr = ReportHelper.getOrgAddressWithPhoneEmail(qc_data.shipment.NotifyPartyAdrs_CompanyName, qc_data.shipment.NotifyPartyAdrs_Address1, qc_data.shipment.NotifyPartyAdrs_Address2, ReportHelper.getCityCountry(qc_data.shipment.NotifyPartyAdrs_City, qc_data.shipment.NotifyPartyAdrs_Country), qc_data.shipment.NotifyPartyAdrs_Email, qc_data.shipment.NotifyPartyAdrs_ContactPhone, qc_data.shipment.NotifyPartyAdrs_ZipPostCode);
//        if(qc_data.shipment.NotifyPartyFullName != null) {
//            notifyPartyAddr.Insert(0, qc_data.shipment.NotifyPartyFullName);
//        }
//        dictionary["NotifyPartyAddress"] = notifyPartyAddr;
//        if (qc_data.shipment.IsNotifyConsigneeEqual.Value)
//        {
//            dictionary["NotifyParty"] = new List<string> { "Same as Consignee" };
//            dictionary["NotifyPartyInCaps"] = new List<string> { "SAME AS CONSIGNEE"};
//        }
//        else
//        {
//            dictionary["NotifyParty"] = dictionary["NotifyPartyAddress"];
//            dictionary["NotifyPartyInCaps"] = notifyPartyAddr?.Select(x => x?.ToUpper()).ToList();
//        }
//
//        if (qc_data.blobject != null & qc_data.shipment.TransportMode != "AIR")
//        {
//            var notify = getNotifyOrgAddress(qc_data.blobject);
//            dictionary[ReportConstants.BL_NOTIFY_PARTY] = notify;
//            dictionary[ReportConstants.BL_NOTIFY_PARTY_CAPS] = notify?.Select(x => x?.ToUpper()).ToList();
//            consigner = getOrgAddress(qc_data.blobject?.ConsignorName, qc_data.blobject?.ConsignorAddress);
//            consignee = getOrgAddress(qc_data.blobject?.ConsigneeName, qc_data.blobject?.ConsigneeAddress);
//        }
//        else
//        {
//            consigner = ReportHelper.getOrgAddressWithPhoneEmail(qc_data.shipment.ConignerAdrs_CompanyName, qc_data.shipment.ConignerAdrs_Address1, qc_data.shipment.ConignerAdrs_Address2, ReportHelper.getCityCountry(qc_data.shipment.ConignerAdrs_City, qc_data.shipment.ConignerAdrs_Country), qc_data.shipment.ConignerAdrs_Email, qc_data.shipment.ConignerAdrs_ContactPhone, qc_data.shipment.ConignerAdrs_ZipPostCode);
//            consignee = ReportHelper.getOrgAddressWithPhoneEmail(qc_data.shipment.ConsigneeAdrs_CompanyName, qc_data.shipment.ConsigneeAdrs_Address1, qc_data.shipment.ConsigneeAdrs_Address2, ReportHelper.getCityCountry(qc_data.shipment.ConsigneeAdrs_City, qc_data.shipment.ConsigneeAdrs_Country), qc_data.shipment.ConsigneeAdrs_Email, qc_data.shipment.ConsigneeAdrs_ContactPhone, qc_data.shipment.ConsigneeAdrs_ZipPostCode);
//        }
//        var delivery = getOrgAddress(qc_data.shipment.NotifyPartyAdrs_CompanyName, qc_data.shipment.NotifyPartyAdrs_Address1, qc_data.shipment.NotifyPartyAdrs_Address2, getCityCountry(qc_data.shipment.NotifyPartyAdrs_City, qc_data.shipment.NotifyPartyAdrs_Country), qc_data.shipment.NotifyPartyAdrs_Email, null);
//        dictionary["Consigner"] = consigner;
//        dictionary["ConsignerInCaps"] = consigner?.Select(x => x?.ToUpper()).ToList();
//        dictionary["ConsignerAddress"] = ReportHelper.getAddressList(qc_data.blobject?.ConsigneeAddress);
//        //dictionary["ConsigneeAddress"] = ReportHelper.getAddressList(qc_data.shipment.ConsigneeAddress1);
//        dictionary["NotifyPartyAddress"] = ReportHelper.getAddressList(qc_data.blobject?.NotifyPartyAddress);
//        string desc = qc_data.blobject != null ? qc_data.blobject?.CargoDescription : qc_data.shipment.Description;
//        dictionary["Description"] = desc;
//        dictionary["DescriptionInCaps"] = desc?.ToUpper();
//        dictionary["DescriptionOriginal"] = ReportHelper.getListFromText(desc);
//        dictionary["Consignee"] = consignee;
//        dictionary["ConsigneeInCaps"] = consignee?.Select(x => x?.ToUpper()).ToList();
//        if (qc_data.shipment.IsNotifyPartyFreeTextAddress.Value)
//        {
//            dictionary["NotifyPartyAddressFreeText"] = ReportHelper.getAddressList(qc_data.shipment.NotifyPartyFreeTextAddress);
//        }
//        else
//        {
//            dictionary["NotifyPartyAddressFreeText"] = dictionary["NotifyParty"];
//        }
//        if (qc_data.shipment.IsConsigneeFreeTextAddress.Value)
//        {
//            dictionary["ConsigneeAddressFreeText"] = ReportHelper.getAddressList(qc_data.shipment.ConsigneeFreeTextAddress);
//        }
//        else
//        {
//            dictionary["ConsigneeAddressFreeText"] = dictionary["Consignee"];
//        }
//
//        if (qc_data.shipment.IsConsignerFreeTextAddress.Value)
//        {
//            dictionary["ConsignerAddressFreeText"] = ReportHelper.getAddressList(qc_data.shipment.ConsignerFreeTextAddress);
//        }
//        else
//        {
//            dictionary["ConsignerAddressFreeText"] = dictionary["Consigner"];
//        }
//        dictionary["OrginalOrCopy"] = "ORIGINAL";
//        dictionary["TenantName"] = qc_data.tenant.TenantName;
//        dictionary["TenantAddress1"] = qc_data.tenant.Address1;
//        dictionary["TenantAddress2"] = qc_data.tenant.Address2;
//        dictionary["TenantEmail"] = qc_data.tenant.Email;
//        dictionary["TenantFax"] = qc_data.tenant.Fax;
//        dictionary["TenantGSTIN"] = qc_data.tenant.VatRegNumber;
//        dictionary["TenantPanNumber"] = qc_data.tenant.PANNumber;
//        dictionary["TenantCity"] = qc_data.tenant.City;
//        dictionary["TenantState"] = qc_data.tenant.State;
//        dictionary["TenantCountry"] = qc_data.tenant.Country;
//        dictionary["TenantContactPhone"] = qc_data.tenant.Phone;
//        dictionary["TenantMobile"] = qc_data.tenant.Mobile;
//        dictionary["TenantZipPostCode"] = qc_data.tenant.ZipPostCode;
//        dictionary["TenantURL"] = qc_data.tenant.WebsiteUrl;
//        dictionary["TenantCompanyRegNumber"] = qc_data.tenant.CompanyRegNumber;
//        dictionary["Tenant"] = ReportHelper.getListOfStrings(qc_data?.tenant?.TenantName, qc_data?.tenant?.Address1, qc_data?.tenant?.Address2, qc_data?.tenant?.City, qc_data?.tenant?.State, qc_data?.tenant?.ZipPostCode, qc_data?.tenant?.Country, qc_data?.tenant?.Email, qc_data?.tenant?.WebsiteUrl, qc_data?.tenant?.Phone);
//        dictionary["BL_VesselName"] = qc_data.blobject?.VesselName;
//        dictionary["BL_Voyage"] = qc_data.blobject?.Voyage;
//        //Shipment Fields
//        dictionary["EntryReferenceNumber"] = qc_data.shipment.EntryRefNo;
//        dictionary["VesselName"] = qc_data.vesselName;
//        dictionary["Voyage"] = qc_data.shipment.Voyage;
//        dictionary["TransportMode"] = qc_data.shipment.TransportMode;
//        dictionary["Description"] = qc_data.blobject != null ? qc_data.blobject?.CargoDescription : qc_data.shipment.Description;
//        dictionary["Chargeable"] = qc_data.shipment.Chargeable;
//        dictionary["ChargeableUnit"] = qc_data.shipment.ChargableUnit;
//        dictionary["FreightOverseas"] = qc_data.shipment.FreightOverseas;
//        dictionary["FreightOverseasCurrency"] = qc_data.shipment.FreightOverseasCurrency;
//        dictionary["Originals"] = qc_data.shipment.Originals;
//        dictionary["OriginalsInWords"] = GetNextNumberHelper.NumberToWords(qc_data.shipment.Originals);
//        dictionary["IssuePlaceName"] = qc_data.shipment.IssuePlaceName;
//        dictionary["IssuePlaceCountry"] = qc_data.shipment.IssuePlaceCountry;
//        dictionary["IssuePlaceCountryName"] = issuePlaceCountryName;
//        dictionary["Bl_Comments"] = qc_data.blobject?.BlComments;
//        dictionary["MarksAndNumbers"] = qc_data.blobject?.MarksAndNumbers;
//        dictionary["MarksAndNumbersInCaps"] = qc_data.blobject?.MarksAndNumbers?.ToUpper();
//        dictionary["ShippedOnBoard"] = qc_data.shipeedOnBoard != null ? Utils.ConvertToDPWDateFormat(qc_data.shipeedOnBoard) : null;
//        dictionary["Packs"] = isHBL == true ? qc_data.blobject?.PackageCount : qc_data.shipment.Packs;
//        dictionary["PacksUnit"] = isHBL == true ? qc_data.blobject?.PackageType : qc_data.shipment.PacksUnit;
//        dictionary["PacksUnitDescription"] = isHBL == true ? masterListDescriptionPacksUnit(qc_data.blobject?.PackageType) : masterListDescriptionPacksUnit(qc_data.shipment.PacksUnit);
//        dictionary["ShipmentContainers"] = qc_data.shipment.ShipmentContainers;
//        dictionary["IsImport"] = qc_data.shipment.Custom_ShipType == "IMP" ? true : false;
//
//        dictionary["DeliveryParty"] = delivery;
//        dictionary["DeliveryPhone"] = qc_data.shipment.NotifyPartyAdrs_ContactPhone;
//        dictionary["DeliveryFax"] = qc_data.shipment.NotifyPartyAdrs_Fax;
//        dictionary["Logo"] = getlogopath(user);
//        var containerCount = 0;
//        foreach (var item in qc_data.shipment.ShipmentContainers)
//        {
//            if (Int32.TryParse(item.ContainerCount, out int currentContainerCount))
//            {
//                containerCount += currentContainerCount;
//            }
//        }
//        dictionary["ContainerCount"] = GetNextNumberHelper.NumberToWords(containerCount).ToUpper();
//        dictionary["CurrentDate"] = Utils.ConvertToDPWDateFormat(System.DateTime.Now);
//        dictionary["HouseBill"] = qc_data.shipment.HouseBill;
//        dictionary["Summary"] = qc_data.shipment.Summary;
//        dictionary["ShipmentId"] = qc_data.shipment.ShipmentId;
//        dictionary["ReferenceNo"] = qc_data.shipment.ReferenceNo;
//        dictionary["ServiceModeDesc"] = serviceModeDesc;
//        dictionary["Goods_CO"] = goods_CO;
//        dictionary["PaymentTerms"] = paymentTerms;
//        dictionary["ReleaseType"] = releaseType;
//        if (qc_data.shipment.Etd != null)
//            dictionary["Etd"] = Utils.ConvertToDPWDateFormat(qc_data.shipment.Etd);
//        if (qc_data.shipment.Eta != null)
//            dictionary["Eta"] = Utils.ConvertToDPWDateFormat(qc_data.shipment.Eta);
//        if (qc_data.shipment.DateofIssue != null){
//            dictionary["DateofIssueMDY"] = Utils.ConvertToDPWDateFormat(qc_data.shipment.DateofIssue);
//            dictionary["DateofIssueDMY"] = ((DateTime)qc_data.shipment.DateofIssue).Date.ToString("dd/MM/yyyy");
//            dictionary["DateofIssueDMMY"] = ((DateTime)qc_data.shipment.DateofIssue).Date.ToString("dd-MMM-yyyy");
//        }
//        if (qc_data.shipment.DateofReceipt != null)
//            dictionary["DateofReceipt"] = Utils.ConvertToDPWDateFormat(qc_data.shipment.DateofReceipt);
//        if (qc_data.shipment.Atd != null){
//            dictionary["AtdMDY"] = Utils.ConvertToDPWDateFormat(qc_data.shipment.Atd);
//            dictionary["AtdDMY"] = ((DateTime)qc_data.shipment.Atd).Date.ToString("dd/MM/yyyy");
//            dictionary["AtdDMMY"] = ((DateTime)qc_data.shipment.Atd).Date.ToString("dd-MMM-yyyy");
//        }
//
//        if (qc_data.shipment.ActualDelivery != null)
//            dictionary["ActualDelivery"] = ((DateTime)qc_data.shipment.ActualDelivery).Date.ToString(Utils.GetDPWDateFormatOrDefault() + " hh:mm tt");
//
//        dictionary["ShipperRefNo"] = qc_data.shipment.ShippersRef;
//        var weight = Math.Round(qc_data.shipment.Weight.GetValueOrDefault(), decimalPlaces);
//        var volume = Math.Round(qc_data.shipment.Volume.GetValueOrDefault(), decimalPlaces);
//        var chargeable = Math.Round(qc_data.shipment.Chargeable.GetValueOrDefault(), decimalPlaces);
//        dictionary["Weight"] = weight;
//        dictionary["Volume"] = volume;
//        dictionary["Chargeable"] = chargeable;
//        dictionary["WeightAndUnit"] = string.Format("{0} {1}", weight, qc_data.shipment.WeightUnit);
//        dictionary["VolumeAndUnit"] = string.Format("{0} {1}", volume, qc_data.shipment.VolumeUnit);
//        dictionary["ChargeableAndUnit"] = string.Format("{0} {1}", chargeable, qc_data.shipment.ChargableUnit);
//        dictionary["DeliveryToEmailAddress"] = DeliveryEmailAddress;
//        dictionary["PlaceOfDelivery"] = qc_data.shipment.PODCountry;
//        dictionary["BL_PlaceOfDelivery"] = qc_data.blobject?.PlaceOfDelivery;
//        dictionary["BL_Weight"] = qc_data.blobject?.CargoGrossWeight;
//        dictionary["BL_WeightUnit"] = qc_data.blobject?.CargoGrossWeightUnit;
//        dictionary["BL_NetWeight"] = qc_data.blobject?.CargoNetWeight;
//        dictionary["BL_NetWeightUnit"] = qc_data.blobject?.CargoNetWeightUnit;
//        dictionary["BL_DeliveryAgent"] = qc_data.blobject?.DeliveryAgent;
//        dictionary["BL_DeliveryAgentAddress"] = qc_data.blobject?.DeliveryAgentAddress;
//        dictionary["BLCargoTermsDescription"] = qc_data.blobject?.CargoTermsDescription;
//        dictionary["BLRemarksDescription"] = qc_data.blobject?.BLRemarksDescription;
//
//
//        if (deliveryToAddress != null)
//        {
//            populateAddress(deliveryToAddress, dictionary, "DeliveryTo");
//            var deliveryTo = getOrgAddress(deliveryToAddress.CompanyName, deliveryToAddress.Address1, deliveryToAddress.Address2, getCityCountry(deliveryToAddress.City, deliveryToAddress.Country), deliveryToAddress.Email, deliveryToAddress.ContactPhone);
//            dictionary["DeliveryTo"] = deliveryTo;
//        }
//        if (deliveryAgentAddress != null && dictionary["DeliveryAgent"] == null)
//        {
//            populateAddress(deliveryAgentAddress, dictionary, "DeliveryAgent");
//            var deliveryAgent = getOrgAddress(deliveryAgentAddress.CompanyName, deliveryAgentAddress.Address1, deliveryAgentAddress.Address2, getCityCountry(deliveryAgentAddress.City, deliveryAgentAddress.Country), deliveryAgentAddress.Email, deliveryAgentAddress.ContactPhone);
//            dictionary["DeliveryAgent"] = deliveryAgent;
//        }
//        if (deliveryTransportCompany != null)
//        {
//            populateAddress(deliveryTransportCompany, dictionary, "DeliveryTransport");
//            var address = getOrgAddress(deliveryTransportCompany.OrgFullName, deliveryTransportCompany.Address1, deliveryTransportCompany.Address2, getCityCountry(deliveryTransportCompany.City, deliveryTransportCompany.Country), deliveryTransportCompany.Email, deliveryTransportCompany.ContactPhone);
//            dictionary["DeliveryTransport"] = address;
//        }
//        if (deliveryCfs != null)
//        {
//            populateAddress(deliveryCfs, dictionary, "DeliveryCfs");
//            var address = getOrgAddress(deliveryCfs.OrgFullName, deliveryCfs.Address1, deliveryCfs.Address2, getCityCountry(deliveryCfs.City, deliveryCfs.Country), deliveryCfs.Email, deliveryCfs.ContactPhone);
//            dictionary["DeliveryCfs"] = address;
//        }
//        if (pickupFrom != null)
//        {
//            populateAddress(pickupFrom, dictionary, "PickupFrom");
//            var address = getOrgAddress(pickupFrom.OrgFullName, pickupFrom.Address1, pickupFrom.Address2, getCityCountry(pickupFrom.City, pickupFrom.Country), pickupFrom.Email, pickupFrom.ContactPhone);
//            dictionary["PickupFrom"] = address;
//        }
//        if (pickupTransportCompany != null)
//        {
//            populateAddress(pickupTransportCompany, dictionary, "PickupTransport");
//            var address = getOrgAddress(pickupTransportCompany.OrgFullName, pickupTransportCompany.Address1, pickupTransportCompany.Address2, getCityCountry(pickupTransportCompany.City, pickupTransportCompany.Country), pickupTransportCompany.Email, pickupTransportCompany.ContactPhone);
//            dictionary["PickupTransport"] = address;
//        }
//        if (pickupCfs != null)
//        {
//            populateAddress(pickupCfs, dictionary, "PickupCfs");
//            var address = getOrgAddress(pickupCfs.OrgFullName, pickupCfs.Address1, pickupCfs.Address2, getCityCountry(pickupCfs.City, pickupCfs.Country), pickupCfs.Email, pickupCfs.ContactPhone);
//            dictionary["PickupCfs"] = address;
//        }
//
//        var ClientAddress = getOrgAddress(qc_data.shipment.ClientAdrs_CompanyName, qc_data.shipment.ClientAdrs_Address1, qc_data.shipment.ClientAdrs_Address2, getCityCountry(qc_data.shipment.ClientAdrs_City, qc_data.shipment.ClientAdrs_Country), null, null);
//        if(qc_data.shipment.ClientFullName != null) {
//            ClientAddress.Insert(0, qc_data.shipment.ClientFullName);
//        }
//        dictionary["ClientAdrs"] = ClientAddress;
//        dictionary["UserFullName"] = Authorization.UserDefinition.DisplayName;
//        dictionary["UserName"] = Authorization.UserDefinition.Username;
//        dictionary["UserEmail"] = Authorization.UserDefinition.Email;
//        if(!string.IsNullOrEmpty(qc_data.shipment.OnBoard)){
//            string OnBoardValue = null;
//            if("SHP".Equals(qc_data.shipment.OnBoard.ToUpper())){
//                OnBoardValue = "Shipped On Board";
//            }
//            if("RFS".Equals(qc_data.shipment.OnBoard.ToUpper())){
//                OnBoardValue = "Received For Shipment";
//            }
//            dictionary["OnBoardType"] = OnBoardValue;
//            dictionary["OnBoardTypeDate"] = qc_data.shipment.OnBoardDate != null ? Utils.ConvertToDPWDateFormat(qc_data.shipment.OnBoardDate) : null;
//        }
//
//        if(!String.IsNullOrEmpty(PrintType)) {
//            if(PrintType.ToUpper() == "ORIGINAL")
//            {
//                dictionary["IsOriginal"] = true;
//            } else
//            {
//                dictionary["IsOriginal"] = false;
//            }
//        }
//
//        if (qc_data.shipment.Atd != null)
//            dictionary["Atd"] = Utils.ConvertToDPWDateFormat(qc_data.shipment.Atd);
//        if (qc_data.shipment.Ata != null)
//            dictionary["Ata"] = Utils.ConvertToDPWDateFormat(qc_data.shipment.Ata);
//
//        dictionary["Attention"] = dictionary["Consignee"];
//        dictionary["DONo"] = qc_data.shipment.ShipmentId;
//        dictionary["Messers"] = qc_data.shipment.ConsigneeFullName;
//        dictionary["IGMNo"] = qc_data.shipment.IGMFileNo;
//        dictionary["FlightName"] = qc_data.shipment.Carrier;
//        dictionary["FlightNo"] = qc_data.shipment.FlightNumber;
//        dictionary["MBLNumber"] = qc_data.shipment.MasterBill;
//        dictionary["HBLNumber"] = qc_data.shipment.HouseBill;
//        if(qc_data.shipment.VolumeWeight != null)
//            dictionary["VWeightAndUnitAir"] = string.Format("{0} {1}", ReportHelper.twoDecimalPlacesFormat(qc_data.shipment.VolumeWeight.ToString()), qc_data.shipment.WeightUnit);
//        if(qc_data.shipment.Weight != null)
//            dictionary["WeightAndUnitAir"] = string.Format("{0} {1}", ReportHelper.twoDecimalPlacesFormat(qc_data.shipment.Weight.ToString()), qc_data.shipment.WeightUnit);
//        if(qc_data.shipment.Volume != null)
//            dictionary["VolumeAndUnitAir"] = string.Format("{0} {1}", ReportHelper.twoDecimalPlacesFormat(qc_data.shipment.Volume.ToString()), qc_data.shipment.VolumeUnit);
//        dictionary["DescofGoods"] = qc_data.shipment.Description;
//        dictionary["JobNo"] = qc_data.shipment.ShipmentId;
//        dictionary["FlightCarrier"] = qc_data.shipment.Carrier;
//        dictionary["PortofLoading"] = qc_data.shipment.POLPortName;
//        dictionary["PortofLoadingCountry"] = qc_data.shipment.POLCountry;
//        dictionary["PortofDischarge"] = qc_data.shipment.PODPortName;
//        dictionary["PortofDischargeCountry"] = qc_data.shipment.PODCountry;
//        dictionary["PlaceofReceipt"] = qc_data.shipment.OriginName;
//        dictionary["PlaceofDelivery"] = qc_data.shipment.DestinationName;
//        dictionary["PortofFinalDestination"] = qc_data.shipment.PODPortName;
//        dictionary["PortofFinalDestinationCountry"] = qc_data.shipment.PODCountry;
//        dictionary["TransportMode"] = qc_data.shipment.TransportMode;
//
//        if(consolRow!=null && consolRow.Payment!=null)
//            dictionary["PPCC"] = consolRow.Payment;
//
//        if(qc_data.shipment.ActualPickup!=null)
//            dictionary["Status"] = "Confirmed";
//        else
//            dictionary["Status"] = "Planned";
//
//        if(packsDetails.Count() > 0)
//        {
//            string json = JsonConvert.SerializeObject(packsDetails);
//            var values = JsonConvert.DeserializeObject<List<Dictionary<string, object>>>(json);
//            using(var connection = SqlConnections.NewByKey("Default"))
//            {
//                values.ForEach(v =>
//                        {
//                                PrintUtil.JsonDateFormat(ref v);
//                if(v.ContainsKey("CommodityName") && v["CommodityName"] != null)
//                {
//                    v["CommodityDesc"] = v["CommodityName"].ToString();
//                }
//
//                if(v["Weight"] != null && v["Weight"].ToString() != null)
//                    v["WeightAndUnitPacks"] = string.Format("{0} {1}", ReportHelper.twoDecimalPlacesFormat(v["Weight"].ToString()), v["WeightUnit"].ToString());
//                if(v["Volume"] != null && v["Volume"].ToString() != null)
//                    v["VolumeAndUnitPacks"] = string.Format("{0} {1}", ReportHelper.twoDecimalPlacesFormat(v["Volume"].ToString()), v["VolumeUnit"].ToString());
//                if(v["VolumeWeight"] != null && v["VolumeWeight"].ToString() != null)
//                    v["VWeightAndUnitPacks"] = string.Format("{0} {1}", ReportHelper.twoDecimalPlacesFormat(v["VolumeWeight"].ToString()), v["WeightUnit"].ToString());
//                if (qc_data.shipment.ActualPickup != null)
//                    v["LoadedDate"] = Utils.ConvertToDPWDateFormat(qc_data.shipment.ActualPickup);
//                    });
//            }
//            dictionary["PacksDetails"] = values;
//        }
//
//        dictionary["PickUpOrderContactPerson"] = string.Empty;
//        if (qc_data.shipment.PickTransportCopantAddressId != null)
//        {
//            if (!string.IsNullOrEmpty(pickupTransportCompany.ContactPerson))
//            {
//                dictionary["PickUpOrderContactPerson"] = pickupTransportCompany.ContactPerson;
//            }
//        }
//        dictionary["ContainerSummary"] = string.Empty;
//        if (qc_data.shipment.ShipmentContainers != null && qc_data.shipment.ShipmentContainers.Count > 0)
//        {
//            var containerSummary = from cont in qc_data.shipment.ShipmentContainers
//            orderby cont.ContainerTypeCode
//            group cont by cont.ContainerTypeCode into containers
//            select new
//            {
//                key = containers.Key,
//                        ContCount = containers.Count(),
//                        CountDesc = string.Format("{0} * {1}", containers.Count(), containers.Key)
//            };
//            var containerCounts = String.Join(",", containerSummary.Select(S => S.CountDesc).ToList());
//            dictionary["ContainerSummary"] = containerCounts;
//        }
//        dictionary["BookingNumber"] = qc_data.shipment.BookingNumber;
//        dictionary["AdditionalTerms"] = qc_data.shipment.AdditionalTerms;
//        dictionary["VesselBerthingDate"] = Utils.ConvertToDPWDateFormat(qc_data.shipment.VesselBerthingDate);
//        //get the pick up associated with shipment id
//        dictionary["UCRReference"] = string.Empty;
//        dictionary["EmptyTruckInDate"] = string.Empty;
//        dictionary["LoadedTruckGateOutDate"] = string.Empty;
//        if (pickupResponse != null)
//        {
//            dictionary["UCRReference"] = pickupResponse.UCRReference;
//            dictionary["EmptyTruckInDate"] = Utils.ConvertToDPWDateFormat(pickupResponse.EmptyTruckInDate);
//            dictionary["LoadedTruckGateOutDate"] = Utils.ConvertToDPWDateFormat(pickupResponse.LoadedTruckGateOutDate);
//            dictionary["PickupPortTransportAdvised"] = Utils.ConvertToDPWDateFormat(pickupResponse.PickupPortTransportAdvised);
//        }
//
//        if (bookingPreCarriageMode != null && bookingPreCarriageMode.Count > 0)
//            dictionary["Precarriagemode"] = String.join(",", bookingPreCarriageMode);
//        if (bookingCarriageVessalVoyage != null && bookingCarriageVessalVoyage.Count > 0)
//            dictionary["Precarriagevesselvoyage"] = String.Join(",", bookingCarriageVessalVoyage);
//
//        return dictionary;    }
//}
