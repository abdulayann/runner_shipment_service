package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import com.dpw.runner.shipment.services.entitytransfer.enums.AddressTypes;
import com.dpw.runner.shipment.services.entitytransfer.enums.RegStatus;
import lombok.*;

import java.time.LocalDateTime;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferAddress implements IEntityTranferBaseEntity {
    public Long OrgId;
    public String AddressShortCode;
    public String CompanyName;
    public AddressTypes AddressType;
    public String SiteIdentifier;
    public String AdditionalAddressInfo;
    public String Address1;
    public String Address2;
    public String Country;
    public String City;
    public String State;
    public String ZipPostCode;
    public String Unloco;
    public String ContactPhone;
    public String Mobile;
    public String Fax;
    public String Email;
    public Boolean Active;
    public Boolean IsObsolete;
    public Boolean IsEdited;
    public String locCode;
    public int CustomerType;
    public String OrgCode;
    public Boolean DefaultAddress;
    public String TaxRegNumber;
    public Boolean CustomClearanceLock;
    public String IECode;
    public String PartyName;
    public String BranchSINumber;
    public String ContactPerson;
    public String OrgPlaceOfSupply;
    public String StateCode;
    public String StateID;
    public RegStatus RegistrationStatus;
    public Boolean TaxExemptionApplicable;
    public LocalDateTime ExemptionStartDate;
    public LocalDateTime ExemptionExpiryDate;
    public String ExemptionSection;
    public int RAKCType;
    public String KCRANumber;
    public LocalDateTime KCRAExpiry;
}
