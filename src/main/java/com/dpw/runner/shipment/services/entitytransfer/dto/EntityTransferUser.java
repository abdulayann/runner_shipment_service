package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.time.LocalDateTime;
@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferUser implements IEntityTranferBaseEntity {
    public Long UserId;
    public String Username;
    public String Source;
    public String PasswordHash;
    public String PasswordSalt;
    public String DisplayName;
    public String Email;
    public String  UserImage;
    public String ImpersonateToken;
    public LocalDateTime LastDirectoryUpdate;
    public String CompanyCurrency;
    public String Password;
    public String OwnCaptcha;
    public String PasswordConfirm;
    public int Platform;
    public String EmployeeToken;
    public String ClearanceUrl;
}

